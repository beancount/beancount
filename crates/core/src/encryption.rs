use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn is_gpg_installed() -> bool {
  let output = Command::new("gpg").arg("--version").output();
  let Ok(output) = output else {
    return false;
  };
  if !output.status.success() {
    return false;
  }
  let version_text = String::from_utf8_lossy(&output.stdout);
  // Match the Python helper: gpg (GnuPG) (1.4|2).*
  version_text.starts_with("gpg (GnuPG) 1.4") || version_text.starts_with("gpg (GnuPG) 2")
}

pub fn is_encrypted_file<P: AsRef<Path>>(filename: P) -> bool {
  let path = filename.as_ref();
  let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");

  if ext.eq_ignore_ascii_case("gpg") {
    return true;
  }

  if ext.eq_ignore_ascii_case("asc") {
    // Python suppresses UnicodeDecodeError and returns False.
    let head = match read_head_ascii(path, 1024) {
      Ok(s) => s,
      Err(_) => return false,
    };
    return head.contains("--BEGIN PGP MESSAGE--");
  }

  false
}

fn read_head_ascii(path: &Path, limit: usize) -> io::Result<String> {
  let bytes = std::fs::read(path)?;
  let head = &bytes[..bytes.len().min(limit)];

  // Mirror Python's `encoding="ascii"` behavior: non-ASCII is treated as an error.
  if head.iter().any(|b| *b >= 0x80) {
    return Err(io::Error::new(io::ErrorKind::InvalidData, "non-ascii head"));
  }

  // SAFETY: we've checked it's ASCII.
  Ok(String::from_utf8_lossy(head).into_owned())
}

pub fn read_encrypted_file<P: AsRef<Path>>(filename: P) -> io::Result<String> {
  let path: PathBuf = std::fs::canonicalize(filename.as_ref()).unwrap_or_else(|_| {
    // Fall back to the provided path; canonicalize can fail for non-existent paths.
    filename.as_ref().to_path_buf()
  });

  let output = Command::new("gpg")
    .arg("--batch")
    .arg("--decrypt")
    .arg(path)
    .output()?;

  if !output.status.success() {
    let code = output.status.code().unwrap_or(-1);
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(io::Error::other(format!(
      "Could not decrypt file ({code}): {stderr}"
    )));
  }

  String::from_utf8(output.stdout)
    .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
}

#[cfg(test)]
mod tests {
  use super::is_encrypted_file;

  #[test]
  fn encrypted_gpg_extension() {
    assert!(is_encrypted_file("/tmp/does-not-need-to-exist.gpg"));
    assert!(is_encrypted_file("/tmp/UPPER.GPG"));
  }

  #[test]
  fn encrypted_asc_header_ascii() {
    let dir = tempfile::tempdir().expect("tempdir");
    let asc = dir.path().join("file.asc");
    std::fs::write(&asc, b"-----BEGIN PGP MESSAGE-----\n...").expect("write asc");
    assert!(is_encrypted_file(&asc));
  }

  #[test]
  fn asc_non_ascii_head_is_not_encrypted() {
    let dir = tempfile::tempdir().expect("tempdir");
    let asc = dir.path().join("bad.asc");
    std::fs::write(&asc, [0xff, 0xfe, 0x00, 0x01]).expect("write asc");
    assert!(!is_encrypted_file(&asc));
  }

  #[test]
  fn other_extensions_are_not_encrypted() {
    let dir = tempfile::tempdir().expect("tempdir");
    let txt = dir.path().join("file.txt");
    std::fs::write(&txt, b"-----BEGIN PGP MESSAGE-----\n...").expect("write txt");
    assert!(!is_encrypted_file(&txt));
  }
}
