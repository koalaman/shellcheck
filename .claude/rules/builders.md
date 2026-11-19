---
paths:
  - "builders/**"
  - ".github/workflows/**"
---

# Builder images

Read `builders/README.md` first. It defines the builder contract, the
preferred build strategies, and how CI uses the images.

## Hard rules

- Builders must run on an x86_64 Linux host with plain Docker. That rules out
  binfmt_misc, `--privileged`, and any non-x86_64 hardware. Don't depend on any
  of these, even if your machine happens to have them.
- Prefer native compilation, then cross-compilation. Use full userspace
  emulation only when a cross-compiler can't be made to work.
- Never push builder images or log in to Docker Hub. CI pulls the published
  images, so a push changes how releases are built. Pushing is the
  maintainer's job.
- A builder's stdout is the artifact tarball. Everything else must go to
  stderr.

## Traps

- CI does not build the images. Editing a `Dockerfile` or `build` script
  changes nothing in CI until the image is rebuilt and pushed.
- `builders/build_builder` replaces the local image under its real
  `koalaman/scbuilder-*` tag. Build experiments under your own tag with
  `docker build -t`, and report every tag you built or replaced.
- Only measure binaries built by the image's own `build` script from a
  `cabal sdist` tarball. A plain `cabal build` skips `striptests`, which leaves
  the unit tests in the binary and makes sizes and timings meaningless.
- Without binfmt_misc, every foreign binary must be launched through the
  emulator explicitly, along with any process it starts. Your development host
  may have binfmt_misc enabled, which hides missing emulator calls. Before you
  claim that emulation works, check `/proc/sys/fs/binfmt_misc/status`. Ask the
  user to disable binfmt_misc if it is on (this needs root).
- Timings under QEMU or Wine are only good for comparing builds with each
  other. Say so when you report them.
- `.gitignore` ignores `cabal.project.freeze`, so add new freeze files under
  `builders/` with `git add -f`.
- `.prepare_deploy`, `Dockerfile.multi-arch` and `.multi_arch_docker` also
  match on builder names (`os.arch`). Update them when you add a target.
