use std::sync::LazyLock;

use pulldown_cmark::{Event, HeadingLevel, Options, Parser, Tag, TagEnd, html};

pub static DOCS: LazyLock<String> =
    LazyLock::new(|| markdown_to_html(include_str!("../../README.md")));

fn h2u(heading: &HeadingLevel) -> usize {
    match heading {
        HeadingLevel::H1 => 1,
        HeadingLevel::H2 => 2,
        HeadingLevel::H3 => 3,
        HeadingLevel::H4 => 4,
        HeadingLevel::H5 => 5,
        HeadingLevel::H6 => 6,
    }
}

const DROPDOWN_HEADING_LEVEL: usize = 3;

fn markdown_to_html(markdown: &str) -> String {
    let parser = Parser::new_ext(markdown, Options::all());

    let mut in_details = false;
    let mut mapped = Vec::new();

    for event in parser {
        match &event {
            Event::Start(Tag::Heading { level, .. }) if h2u(level) == DROPDOWN_HEADING_LEVEL => {
                if in_details {
                    mapped.push(Event::Html("</details>".into()));
                    in_details = false;
                }
                mapped.push(event);
            }
            Event::End(TagEnd::Heading(level)) if h2u(level) == DROPDOWN_HEADING_LEVEL => {
                mapped.push(event);
                mapped.push(Event::Html("<details>".into()));
                in_details = true;
            }
            Event::Start(Tag::Heading { level, .. })
                if in_details && h2u(level) <= DROPDOWN_HEADING_LEVEL =>
            {
                mapped.push(Event::Html("</details><br></br>".into()));
                in_details = false;
                mapped.push(event);
            }
            _ => mapped.push(event),
        }
    }

    // Close if file ended while still inside details
    if in_details {
        mapped.push(Event::Html("</details>".into()));
    }

    let mut html_output = String::new();
    html::push_html(&mut html_output, mapped.into_iter());

    html_output
}
