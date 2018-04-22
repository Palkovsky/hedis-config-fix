# CHANGELOG

## 1.0.0
### Changed
* Compatibility with `hedis >= 0.9.12`
* Not compatible with `hedis < 0.9.12`
### Added
* Connection timeout parameter: `timeout`

## 0.0.3
### Changed
* Empty password string will be nullified. Redis counts empty `AUTH` command as
  error when no password is set.

## 0.0.2
### Changed
* relaxed upper base version constraints to `< 5`

## 0.0.1
First working version
