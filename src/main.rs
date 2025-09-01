use leptos::prelude::*;

mod app;
use app::App;

pub fn main() {
    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();

    mount_to_body(|| view! { <App /> });
}

pub fn image_url(image: &str) -> String {
    let url = window().location().href().ok().unwrap_or(String::new());
    url + "/public/" + image
}
