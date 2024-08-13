project(
  'SIFDecode',
  'fortran',
  version: '2.5.0',
  meson_version: '>= 0.61.0',
  default_options: [
    'buildtype=release',
    'libdir=lib',
    'default_library=shared',
    'warning_level=0',
  ],
)

libsifdecode_src = []
sifdecode_binaries = []

# Sources
subdir('src/check')
subdir('src/decode')
subdir('src/select')

# Library
libsifdecode = library('sifdecode',
                       sources : libsifdecode_src,
                       install : true)

# Binaries
foreach binary: sifdecode_binaries
  binname = binary[0]
  binfile = binary[1]
  executable(binname,
             sources : binfile,
             link_with : libsifdecode,
             install : true)
endforeach

# Fortran modules
script_modules = files('install_modules.py')
meson.add_install_script(script_modules)
