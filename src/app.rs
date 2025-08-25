use leptos::prelude::*;

use crate::interpreter::run_code;

#[component]
pub fn App() -> impl IntoView {
    let (code, set_code) = signal("".to_string());
    let (output, set_output) = signal(vec![(0, "".to_string())]);

    view! {
        <textarea
            class:code-input
            on:input:target=move |ev| {
                set_code.set(ev.target().value());
            }
            prop:value=code
        />
        <br />
        <button on:click=move |_| {
            log::debug!("Running code now");
            set_output.set(vec![]);
            let res = run_code(
                &code.get(),
                None,
                Box::new(move |out| {
                    let mut set_out_tmp = set_output.write();
                    for line in out.split("\n") {
                        log::debug!("new line {}", line);
                        let len = set_out_tmp.len() + 1;
                        set_out_tmp.push((len, line.to_string()));
                    }
                }),
            );
            match res {
                Ok(_) => {}
                Err(err) => {
                    let mut set_out_tmp = set_output.write();
                    for line in err.write(&code.get()).split("\n") {
                        log::debug!("new line {}", line);
                        let len = set_out_tmp.len() + 1;
                        set_out_tmp.push((len, line.to_string()));
                    }
                }
            }
            log::debug!("{:?}", output.get());
            log::debug!("Finished running code");
        }>"Run Code"</button>
        <div>
            <For each=move || output.get() key=move |i| i.0 let((_, line))>
                {line}
                <br />
            </For>
        </div>
    }
}
