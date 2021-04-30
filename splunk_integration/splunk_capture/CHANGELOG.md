# CHANGELOG

## [v014]
### Updated
- default HEC host to load balanced host: https://splunk-segue-fwd.csg.apple.com:8088

## [v013]
### Updated
- rc authentication option alternative to keyring: https://stackoverflow.com/questions/11030552/what-does-rc-mean-in-dot-files
Make file only readable by yourself.
   - ${HOME}/.splunkrc file format
      - server https://insights.csg.apple.com
      - user <splunk_username>
      - passwd <splunk_password>
- Lookup feature: ability to upload CSV lookups to Splunk. 
   - The files must be validated against a JSON schema. 
   - The JSON schema is a simple object with a key for each CSV header, and a regex expression for what the associated header values must match against.
- input_type featue: ability to specify file type of --input argument (supported formats: csv, xlsx, mxd, json).
- deleteall feature: ability to delete all entries in KV Store.
- Bug fixes, code refinements.
- Full functional test suite.

## [v012]

## [v011]

## [v010] - 2019-06-17
### Updated
- Set add_metadata to false
- Fix template path in monitor email

## [v009] - 2019-06-03
### Added
- Auto-detection of index names from project name 
  - Call to ```splunk_index``` to get index names and monitor directory paths for a project & flow combination 
  - project default is PROJECT env variable value and flow default is "main" (Eg. prj_skua_main)
- New --monitor option 
  - When called with monitor option, the input file is copied to site specific splunk monitor directory
  - Can also be called by setting SPLUNK_CAPTURE_MONITOR env variable to true (Eg. env SPLUNK_CAPTURE_MONITOR=1 ...)

### Removed
- splunk_capture email options. Moved to splunk_mailer
- ```--list``` indexes from splunk_capture event

