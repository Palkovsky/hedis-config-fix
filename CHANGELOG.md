# CHANGELOG

## 0.03
### Changed
* Empty password string will be nullified. Redis counts empty `AUTH` command as
  error when no password is set.

## 0.0.2
### Changed
* relaxed upper base version constraints to `< 5`

## 0.0.1
First working version
