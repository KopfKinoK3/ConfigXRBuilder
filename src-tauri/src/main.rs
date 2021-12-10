#![cfg_attr(
  all(not(debug_assertions) /*, target_os = "macos", tartget_arch = "aarch64" */),
  windows_subsystem = "windows"
)]


fn main() {
  tauri::Builder::default()
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
}
