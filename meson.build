project(
  'SIFDecode',
  'fortran',
  version: '2.6.4',
  meson_version: '>= 0.61.0',
  default_options: [
    'buildtype=release',
    'libdir=lib',
    'default_library=static',
    'warning_level=0',
  ],
)

sifdecode_binaries = []

# Sources
subdir('src/decode')
subdir('src/select')

# Binaries
foreach binary: sifdecode_binaries
  binname = binary[0]
  binfile = binary[1]
  executable(binname,
             sources : binfile,
             install : true)
endforeach

# Install sifdecoder man page
install_data(
  'man/man1/sifdecoder_standalone.1',
  install_dir: get_option('mandir') / 'man1',
  rename: 'sifdecoder.1'
)
