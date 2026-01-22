use anyhow::{Context, Result, bail};
use chrono::{Local, NaiveTime};
use clap::Parser;
use crossterm::{
    ExecutableCommand,
    event::{self, Event, KeyCode, KeyEventKind},
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    Frame, Terminal,
    layout::{Constraint, Layout, Rect},
    style::{Color, Modifier, Style, Stylize},
    text::{Line, Span},
    widgets::{Block, Borders, Cell, Paragraph, Row as TableRow, Table},
};
use roxmltree::{Document, Node};
use std::io::stdout;
use std::time::Duration;
use tokio::time::Instant;

const ENDPOINT: &str = "https://lite.realtime.nationalrail.co.uk/OpenLDBWS/ldb12.asmx";

/// Live departures between two CRS stations using National Rail Darwin OpenLDBWS (SOAP).
#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// From CRS (e.g. PAD)
    #[arg(long)]
    from: String,

    /// To CRS (e.g. RDG)
    #[arg(long)]
    to: String,

    /// Train Operating Company code (optional, shows all if not specified)
    #[arg(long)]
    toc: Option<String>,

    /// Number of rows to return (max 150, but keep it sensible)
    #[arg(long, default_value_t = 10)]
    rows: u32,

    /// Minutes into the future to include (timeWindow)
    #[arg(long, default_value_t = 120)]
    window_mins: u32,

    /// Minutes offset from "now" (timeOffset)
    #[arg(long, default_value_t = 0)]
    offset_mins: i32,

    /// Refresh interval in seconds
    #[arg(long, default_value_t = 30)]
    refresh: u64,
}

struct App {
    from: String,
    to: String,
    toc: Option<String>,
    rows: u32,
    window_mins: u32,
    offset_mins: i32,
    token: String,
    client: reqwest::Client,
    refresh_interval: Duration,

    // State
    location_name: String,
    filter_location_name: String,
    generated_at: String,
    services: Vec<ServiceRow>,
    last_refresh: Instant,
    error: Option<String>,
}

#[derive(Debug, Clone)]
struct ServiceRow {
    std: String,
    etd: String,
    status: Status,
    arr_st: Option<String>,
    arr_et: String,
    journey: String,
    platform: String,
    destination: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Status {
    OnTime,
    Late(i64),
    Early(i64),
    Cancelled,
    Delayed,
    Unknown,
}

impl App {
    fn new(args: &Args, token: String) -> Result<Self> {
        let from = normalize_crs(&args.from)?;
        let to = normalize_crs(&args.to)?;
        let toc = args.toc.as_ref().map(|s| s.trim().to_ascii_uppercase());

        Ok(Self {
            from: from.clone(),
            to: to.clone(),
            toc,
            rows: args.rows,
            window_mins: args.window_mins,
            offset_mins: args.offset_mins,
            token,
            client: reqwest::Client::new(),
            refresh_interval: Duration::from_secs(args.refresh),

            location_name: from,
            filter_location_name: to,
            generated_at: String::new(),
            services: Vec::new(),
            last_refresh: Instant::now() - Duration::from_secs(1000),
            error: None,
        })
    }

    async fn refresh(&mut self) {
        self.last_refresh = Instant::now();
        self.error = None;

        match self.fetch_departures().await {
            Ok((location, filter_location, generated, services)) => {
                self.location_name = location;
                self.filter_location_name = filter_location;
                self.generated_at = generated;
                self.services = services;
            }
            Err(e) => {
                self.error = Some(format!("{e:#}"));
            }
        }
    }

    async fn fetch_departures(&self) -> Result<(String, String, String, Vec<ServiceRow>)> {
        let soap = build_request(
            &self.token,
            self.rows,
            &self.from,
            &self.to,
            self.offset_mins,
            self.window_mins,
        );

        let response = self
            .client
            .post(ENDPOINT)
            .header(
                "Content-Type",
                "application/soap+xml; charset=utf-8; action=\"http://thalesgroup.com/RTTI/2015-05-14/ldb/GetDepBoardWithDetails\"",
            )
            .body(soap)
            .send()
            .await
            .context("SOAP request failed")?;

        let status = response.status();
        let resp = response
            .text()
            .await
            .context("Failed reading SOAP response body")?;

        if !status.is_success() {
            bail!("SOAP returned {status}: {resp}");
        }

        let doc = Document::parse(&resp).context("Failed parsing XML")?;

        let location = find_text(&doc, "locationName").unwrap_or_else(|| self.from.clone());
        let filter_location = find_text(&doc, "filterLocationName").unwrap_or_else(|| self.to.clone());
        let generated = find_text(&doc, "generatedAt")
            .map(|s| format_timestamp(&s))
            .unwrap_or_else(|| "-".into());

        let services = extract_services(&doc, &self.to, self.toc.as_deref())?;

        Ok((location, filter_location, generated, services))
    }

    fn should_refresh(&self) -> bool {
        self.last_refresh.elapsed() >= self.refresh_interval
    }

    fn time_until_refresh(&self) -> Duration {
        self.refresh_interval
            .saturating_sub(self.last_refresh.elapsed())
    }
}

fn format_timestamp(ts: &str) -> String {
    // Parse ISO timestamp and extract time portion
    if let Some(t_pos) = ts.find('T') {
        let time_part = &ts[t_pos + 1..];
        if let Some(dot_pos) = time_part.find('.') {
            return time_part[..dot_pos].to_string();
        }
        if let Some(plus_pos) = time_part.find('+') {
            return time_part[..plus_pos].to_string();
        }
    }
    ts.to_string()
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let token = std::env::var("NRE_LDBWS_TOKEN").context("Missing env var NRE_LDBWS_TOKEN")?;

    let mut app = App::new(&args, token)?;

    // Setup terminal
    enable_raw_mode()?;
    stdout().execute(EnterAlternateScreen)?;
    let mut terminal = Terminal::new(ratatui::backend::CrosstermBackend::new(stdout()))?;

    let result = run_app(&mut terminal, &mut app).await;

    // Restore terminal
    disable_raw_mode()?;
    stdout().execute(LeaveAlternateScreen)?;

    result
}

async fn run_app(
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<std::io::Stdout>>,
    app: &mut App,
) -> Result<()> {
    loop {
        // Check if we need to refresh
        if app.should_refresh() {
            app.refresh().await;
        }

        // Draw UI
        terminal.draw(|frame| draw_ui(frame, app))?;

        // Handle input with timeout
        let timeout = Duration::from_millis(100);
        if event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    match key.code {
                        KeyCode::Char('q') | KeyCode::Esc => return Ok(()),
                        KeyCode::Char('r') => {
                            app.refresh().await;
                        }
                        _ => {}
                    }
                }
            }
        }
    }
}

fn draw_ui(frame: &mut Frame, app: &App) {
    let area = frame.area();

    let chunks = Layout::vertical([
        Constraint::Length(3), // Header
        Constraint::Min(5),    // Table
        Constraint::Length(3), // Footer
    ])
    .split(area);

    draw_header(frame, app, chunks[0]);
    draw_table(frame, app, chunks[1]);
    draw_footer(frame, app, chunks[2]);
}

fn draw_header(frame: &mut Frame, app: &App, area: Rect) {
    let title = format!(" {} ({}) → {} ({}) ", app.location_name, app.from, app.filter_location_name, app.to);

    let info = match &app.toc {
        Some(toc) => format!("TOC: {} | Updated: {}", toc, app.generated_at),
        None => format!("Updated: {}", app.generated_at),
    };

    let header = Paragraph::new(vec![
        Line::from(vec![Span::styled(
            title,
            Style::default().fg(Color::Cyan).bold(),
        )]),
        Line::from(vec![Span::styled(
            info,
            Style::default().fg(Color::DarkGray),
        )]),
    ])
    .block(
        Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Blue))
            .title(" Departures ")
            .title_style(Style::default().fg(Color::Yellow).bold()),
    );

    frame.render_widget(header, area);
}

fn draw_table(frame: &mut Frame, app: &App, area: Rect) {
    if let Some(ref err) = app.error {
        let error_para = Paragraph::new(vec![
            Line::from(""),
            Line::from(Span::styled(
                format!("Error: {err}"),
                Style::default().fg(Color::Red),
            )),
        ])
        .block(
            Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Red)),
        );
        frame.render_widget(error_para, area);
        return;
    }

    if app.services.is_empty() {
        let empty = Paragraph::new(vec![
            Line::from(""),
            Line::from(Span::styled(
                "No matching services in window",
                Style::default().fg(Color::Yellow),
            )),
        ])
        .block(Block::default().borders(Borders::ALL));
        frame.render_widget(empty, area);
        return;
    }

    let header = TableRow::new(vec![
        Cell::from("Plat").style(Style::default().bold()),
        Cell::from("Departs").style(Style::default().bold()),
        Cell::from("Status").style(Style::default().bold()),
        Cell::from("Arrives").style(Style::default().bold()),
        Cell::from("Journey").style(Style::default().bold()),
        Cell::from("Destination").style(Style::default().bold()),
    ])
    .style(Style::default().fg(Color::Yellow))
    .height(1);

    let rows: Vec<TableRow> = app
        .services
        .iter()
        .map(|s| {
            let status_style = match s.status {
                Status::OnTime => Style::default().fg(Color::Green),
                Status::Late(_) => Style::default().fg(Color::Red),
                Status::Early(_) => Style::default().fg(Color::Cyan),
                Status::Cancelled => Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
                Status::Delayed => Style::default().fg(Color::Yellow),
                Status::Unknown => Style::default().fg(Color::DarkGray),
            };

            let status_text = match s.status {
                Status::OnTime => "On time".to_string(),
                Status::Late(m) => format!("+{m} min"),
                Status::Early(m) => format!("{m} min"),
                Status::Cancelled => "CANCELLED".to_string(),
                Status::Delayed => "Delayed".to_string(),
                Status::Unknown => "-".to_string(),
            };

            let departs = if s.std == s.etd || s.etd == "On time" {
                s.std.clone()
            } else {
                format!("{} → {}", s.std, s.etd)
            };

            let arrives = if s.arr_st.as_deref() == Some(&s.arr_et) || s.arr_et == "On time" {
                s.arr_st.clone().unwrap_or_else(|| "-".to_string())
            } else if let Some(ref st) = s.arr_st {
                format!("{} → {}", st, s.arr_et)
            } else {
                "-".to_string()
            };

            TableRow::new(vec![
                Cell::from(s.platform.clone()).style(Style::default().fg(Color::Cyan).bold()),
                Cell::from(departs),
                Cell::from(status_text).style(status_style),
                Cell::from(arrives),
                Cell::from(s.journey.clone()).style(Style::default().fg(Color::DarkGray)),
                Cell::from(s.destination.clone()),
            ])
            .height(1)
        })
        .collect();

    let table = Table::new(
        rows,
        [
            Constraint::Length(5),  // Platform
            Constraint::Length(14), // Departs
            Constraint::Length(10), // Status
            Constraint::Length(14), // Arrives
            Constraint::Length(11), // Journey
            Constraint::Min(15),    // Destination
        ],
    )
    .header(header)
    .block(Block::default().borders(Borders::ALL))
    .row_highlight_style(Style::default().add_modifier(Modifier::BOLD));

    frame.render_widget(table, area);
}

fn draw_footer(frame: &mut Frame, app: &App, area: Rect) {
    let refresh_secs = app.time_until_refresh().as_secs();
    let refresh_text = if refresh_secs == 0 {
        "Refreshing...".to_string()
    } else {
        format!("Next refresh in {refresh_secs}s")
    };

    let footer = Paragraph::new(Line::from(vec![
        Span::styled(" [R] ", Style::default().fg(Color::Yellow).bold()),
        Span::raw("Refresh  "),
        Span::styled("[Q/Esc] ", Style::default().fg(Color::Yellow).bold()),
        Span::raw("Quit  "),
        Span::styled("│ ", Style::default().fg(Color::DarkGray)),
        Span::styled(refresh_text, Style::default().fg(Color::DarkGray)),
    ]))
    .block(
        Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)),
    );

    frame.render_widget(footer, area);
}

fn normalize_crs(s: &str) -> Result<String> {
    let t = s.trim().to_ascii_uppercase();
    if t.len() != 3 || !t.chars().all(|c| c.is_ascii_alphabetic()) {
        bail!("CRS must be 3 letters (e.g. PAD, RDG). Got: {s}");
    }
    Ok(t)
}

fn build_request(
    token: &str,
    num_rows: u32,
    crs: &str,
    filter_crs: &str,
    time_offset: i32,
    time_window: u32,
) -> String {
    format!(
        r#"<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope"
               xmlns:typ="http://thalesgroup.com/RTTI/2013-11-28/Token/types"
               xmlns:ldb="http://thalesgroup.com/RTTI/2021-11-01/ldb/">
  <soap:Header>
    <typ:AccessToken>
      <typ:TokenValue>{token}</typ:TokenValue>
    </typ:AccessToken>
  </soap:Header>
  <soap:Body>
    <ldb:GetDepBoardWithDetailsRequest>
      <ldb:numRows>{num_rows}</ldb:numRows>
      <ldb:crs>{crs}</ldb:crs>
      <ldb:filterCrs>{filter_crs}</ldb:filterCrs>
      <ldb:filterType>to</ldb:filterType>
      <ldb:timeOffset>{time_offset}</ldb:timeOffset>
      <ldb:timeWindow>{time_window}</ldb:timeWindow>
    </ldb:GetDepBoardWithDetailsRequest>
  </soap:Body>
</soap:Envelope>
"#
    )
}

fn extract_services(doc: &Document, to_crs: &str, toc: Option<&str>) -> Result<Vec<ServiceRow>> {
    let mut rows = Vec::new();

    for service in doc
        .descendants()
        .filter(|n| n.is_element() && local_name(n) == "service")
    {
        let std = child_text(service, "std");
        let etd_raw = child_text(service, "etd");
        let operator_code = child_text(service, "operatorCode");

        let Some(std) = std else { continue };

        if let Some(toc) = toc {
            let Some(ref op) = operator_code else { continue };
            if op.trim().to_ascii_uppercase() != toc {
                continue;
            }
        }

        let dest_name = service
            .descendants()
            .find(|n| n.is_element() && local_name(n) == "destination")
            .and_then(|dest| {
                dest.descendants()
                    .find(|n| n.is_element() && local_name(n) == "locationName")
                    .and_then(|n| n.text())
                    .map(|s| s.to_string())
            })
            .unwrap_or_else(|| "-".to_string());

        let platform = child_text(service, "platform").unwrap_or_else(|| "-".to_string());
        let (arr_st, arr_et) = find_destination_calling_point(service, to_crs);

        let etd = etd_raw.clone().unwrap_or_else(|| "-".into());
        let status = compute_status(&std, etd_raw.as_deref());
        let journey = format_journey(
            &std,
            etd_raw.as_deref(),
            arr_st.as_deref(),
            arr_et.as_deref(),
        );

        rows.push(ServiceRow {
            std,
            etd,
            status,
            arr_st,
            arr_et: arr_et.unwrap_or_else(|| "-".to_string()),
            journey,
            platform,
            destination: dest_name,
        });
    }

    let now = Local::now().time();
    rows.sort_by_key(|r| {
        let time = parse_hhmm(&r.std).unwrap_or(NaiveTime::from_hms_opt(23, 59, 59).unwrap());
        let mut diff = (time - now).num_minutes();
        if diff < -360 {
            diff += 1440; // treat times >6 hours ago as tomorrow
        }
        diff
    });

    Ok(rows)
}

fn compute_status(std: &str, etd_raw: Option<&str>) -> Status {
    let raw = match etd_raw.map(|s| s.trim()) {
        Some("") | None => return Status::Unknown,
        Some(r) => r,
    };

    if raw.eq_ignore_ascii_case("on time") {
        return Status::OnTime;
    }
    if raw.eq_ignore_ascii_case("cancelled") {
        return Status::Cancelled;
    }
    if raw.eq_ignore_ascii_case("delayed") {
        return Status::Delayed;
    }

    if let (Some(scheduled), Some(expected)) = (parse_hhmm(std), parse_hhmm(raw)) {
        let mut diff = (expected - scheduled).num_minutes();
        if diff < -720 {
            diff += 1440;
        }
        if diff == 0 {
            Status::OnTime
        } else if diff > 0 {
            Status::Late(diff)
        } else {
            Status::Early(diff)
        }
    } else {
        Status::Unknown
    }
}

fn find_destination_calling_point(service: Node, to_crs: &str) -> (Option<String>, Option<String>) {
    for cp in service
        .descendants()
        .filter(|n| n.is_element() && local_name(n) == "callingPoint")
    {
        let crs = child_text(cp, "crs").unwrap_or_default();
        if crs.trim().eq_ignore_ascii_case(to_crs) {
            let st = child_text(cp, "st");
            let et = child_text(cp, "et");
            return (st, et);
        }
    }
    (None, None)
}

fn format_journey(
    std: &str,
    etd_raw: Option<&str>,
    st: Option<&str>,
    et_raw: Option<&str>,
) -> String {
    let sched = match (parse_hhmm(std), st.and_then(parse_hhmm)) {
        (Some(dep), Some(arr)) => Some(mins_wrap(dep, arr)),
        _ => None,
    };

    let exp = match (
        interpret_expected_time(std, etd_raw),
        interpret_expected_time_opt(st, et_raw),
    ) {
        (Expected::Time(dep_e), ExpectedOpt::Time(arr_e)) => {
            match (parse_hhmm(&dep_e), parse_hhmm(&arr_e)) {
                (Some(d), Some(a)) => Some(mins_wrap(d, a)),
                _ => None,
            }
        }
        _ => None,
    };

    match (sched, exp) {
        (Some(s), Some(e)) if s != e => format!("{s}m → {e}m"),
        (Some(s), _) => format!("{s}m"),
        _ => "-".to_string(),
    }
}

fn mins_wrap(dep: NaiveTime, arr: NaiveTime) -> i64 {
    let mut d = (arr - dep).num_minutes();
    if d < -720 {
        d += 1440;
    }
    d
}

#[derive(Debug)]
enum Expected {
    Time(String),
    Cancelled,
    Delayed,
    Unknown,
}

#[derive(Debug)]
enum ExpectedOpt {
    Time(String),
    Other,
    Unknown,
}

fn interpret_expected_time(std: &str, raw: Option<&str>) -> Expected {
    let raw = match raw.map(|s| s.trim()) {
        Some("") | None => return Expected::Unknown,
        Some(r) => r,
    };

    if raw.eq_ignore_ascii_case("on time") {
        return Expected::Time(std.to_string());
    }
    if raw.eq_ignore_ascii_case("cancelled") {
        return Expected::Cancelled;
    }
    if raw.eq_ignore_ascii_case("delayed") {
        return Expected::Delayed;
    }
    if parse_hhmm(raw).is_some() {
        return Expected::Time(raw.to_string());
    }
    Expected::Unknown
}

fn interpret_expected_time_opt(st: Option<&str>, raw: Option<&str>) -> ExpectedOpt {
    let st = match st {
        Some(s) if !s.trim().is_empty() => s.trim(),
        _ => return ExpectedOpt::Unknown,
    };
    let raw = match raw.map(|s| s.trim()) {
        Some("") | None => return ExpectedOpt::Unknown,
        Some(r) => r,
    };

    if raw.eq_ignore_ascii_case("on time") {
        return ExpectedOpt::Time(st.to_string());
    }
    if parse_hhmm(raw).is_some() {
        return ExpectedOpt::Time(raw.to_string());
    }
    ExpectedOpt::Other
}

fn parse_hhmm(s: &str) -> Option<NaiveTime> {
    NaiveTime::parse_from_str(s.trim(), "%H:%M").ok()
}

fn local_name<'a>(n: &'a Node<'a, 'a>) -> &'a str {
    n.tag_name().name()
}

fn find_text(doc: &Document, tag: &str) -> Option<String> {
    doc.descendants()
        .find(|n| n.is_element() && local_name(n) == tag)
        .and_then(|n| n.text())
        .map(|s| s.to_string())
}

fn child_text(parent: Node, tag: &str) -> Option<String> {
    parent
        .children()
        .find(|n| n.is_element() && local_name(n) == tag)
        .and_then(|n| n.text())
        .map(|s| s.to_string())
}
