use anyhow::{anyhow, Result};
use chrono::{DateTime, Duration, NaiveDate, TimeZone, Utc};

pub fn resolve_events_window(
    since: Option<&str>,
    until: Option<&str>,
) -> Result<(DateTime<Utc>, DateTime<Utc>)> {
    let now = Utc::now();
    let default_start = now - Duration::days(1);
    let start = match since {
        Some(raw) => parse_datetime_input(raw)?,
        None => default_start,
    };
    let end = match until {
        Some(raw) => parse_datetime_input(raw)?,
        None => now,
    };

    if end <= start {
        anyhow::bail!("`until` must be after `since`");
    }
    Ok((start, end))
}

pub fn resolve_llm_window(
    selector: &str,
    since: Option<&str>,
    until: Option<&str>,
) -> Result<(DateTime<Utc>, DateTime<Utc>)> {
    let mut bounds = default_window_for_selector(selector)?;

    if selector == "custom" && since.is_none() && until.is_none() {
        anyhow::bail!("custom window requires --since and/or --until");
    }

    if let Some(raw) = since {
        bounds.0 = parse_datetime_input(raw)?;
    }
    if let Some(raw) = until {
        bounds.1 = parse_datetime_input(raw)?;
    }
    if bounds.1 <= bounds.0 {
        anyhow::bail!("window end must be after start");
    }
    Ok(bounds)
}

pub fn default_window_for_selector(selector: &str) -> Result<(DateTime<Utc>, DateTime<Utc>)> {
    let now = Utc::now();
    match selector {
        "yesterday" => {
            let yesterday = now.date_naive() - Duration::days(1);
            let start = Utc.from_utc_datetime(
                &yesterday
                    .and_hms_opt(0, 0, 0)
                    .ok_or_else(|| anyhow!("invalid midnight for yesterday"))?,
            );
            Ok((start, start + Duration::days(1)))
        }
        "last-week" => Ok((now - Duration::days(7), now)),
        "custom" => Ok((now - Duration::days(1), now)),
        other => Err(anyhow!("unknown window selector `{}`", other)),
    }
}

pub fn parse_datetime_input(raw: &str) -> Result<DateTime<Utc>> {
    if let Ok(date) = NaiveDate::parse_from_str(raw, "%Y-%m-%d") {
        let naive = date
            .and_hms_opt(0, 0, 0)
            .ok_or_else(|| anyhow!("invalid midnight for date {}", raw))?;
        return Ok(DateTime::<Utc>::from_naive_utc_and_offset(naive, Utc));
    }

    DateTime::parse_from_rfc3339(raw)
        .map(|dt| dt.with_timezone(&Utc))
        .map_err(|err| anyhow!("failed to parse `{}` as ISO datetime: {}", raw, err))
}
