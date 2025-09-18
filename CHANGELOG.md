# `nbparts` Changelog

## v0.1.1.0

- Fix incorrect serialisation of `FormatJson` in the manifest file.

  - For backward compatibility, `nbparts` will still be able to deserialise the incorrect representation.

- Media directories (`media` and `outputs-media`) will now only be created if necessary (i.e., when there are media to export).

- Packing will no longer fail when the outputs file is missing. Instead, `nbparts` will warn and assume that the notebook has no outputs.

## v0.1.0.0

- Initial release.
