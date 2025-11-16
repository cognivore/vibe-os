pub use core_types::Domain;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainDescriptor {
    pub id: Domain,
    pub human_name: String,
    pub description: String,
}

pub fn builtin_domains() -> Vec<DomainDescriptor> {
    vec![
        DomainDescriptor {
            id: Domain::Slack,
            human_name: "Slack".into(),
            description: "Chat/messages and threads".into(),
        },
        DomainDescriptor {
            id: Domain::Linear,
            human_name: "Linear".into(),
            description: "Issues / project management".into(),
        },
        DomainDescriptor {
            id: Domain::Github,
            human_name: "GitHub".into(),
            description: "Source control and pull requests".into(),
        },
        DomainDescriptor {
            id: Domain::Grain,
            human_name: "Grain".into(),
            description: "Meeting recordings and notes".into(),
        },
        DomainDescriptor {
            id: Domain::GoogleCalendar,
            human_name: "Google Calendar".into(),
            description: "Calendar events".into(),
        },
    ]
}
