use leptos::prelude::*;

mod app;

use app::App;

pub fn main() {
    mount_to_body(|| view! {
        <App />
    });

    _ = console_log::init_with_level(log::Level::Debug);
    console_error_panic_hook::set_once();
}


