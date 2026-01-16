use path_clean::PathClean;
use std::path::Path;

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn resolve_path(base_filename: &str, filename: &str) -> String {
  let path = Path::new(filename);
  if path.is_absolute() {
    return filename.to_string();
  }

  let mut base_dir = Path::new(base_filename)
    .parent()
    .unwrap_or_else(|| Path::new(""))
    .to_path_buf();

  if (base_dir.as_os_str().is_empty() || base_filename.starts_with('<'))
    && let Ok(cwd) = std::env::current_dir()
  {
    base_dir = cwd;
  }

  base_dir.join(path).clean().to_string_lossy().into_owned()
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn resolve_path(base_filename: &str, filename: &str) -> String {
  if filename.is_empty() {
    return base_filename.to_string();
  }

  if base_filename.is_empty() {
    return filename.to_string();
  }

  let path = Path::new(filename);
  if path.is_absolute()
    || filename.starts_with("./")
    || filename.starts_with("../")
    || filename.starts_with('~')
    || filename.contains(':')
  {
    return filename.to_string();
  }

  let base_dir = Path::new(base_filename)
    .parent()
    .unwrap_or_else(|| Path::new(""));
  base_dir.join(path).clean().to_string_lossy().into_owned()
}
