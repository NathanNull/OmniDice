use leptos::prelude::*;

mod app;

use app::App;
use leptos_use::use_favicon;

pub fn main() {
    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();

    mount_to_body(|| view! { <App /> });

    let (_, set_icon) = use_favicon();
    set_icon.set(Some(image_url("omnidice-logo.ico")));
}

pub fn image_url(image: &str) -> String {
    let url = window().location().href().ok().unwrap_or(String::new());
    url + "/public/" + image
}
