using BinaryBuilder, Pkg

haskey(ENV, "SIFDECODE_RELEASE") || error("The environment variable SIFDECODE_RELEASE is not defined.")
haskey(ENV, "SIFDECODE_COMMIT") || error("The environment variable SIFDECODE_COMMIT is not defined.")

name = "SIFDecode"
version = VersionNumber(ENV["SIFDECODE_RELEASE"])

# Collection of sources required to complete build
sources = [
    GitSource("https://github.com/ralna/SIFDecode.git", ENV["SIFDECODE_COMMIT"])
]

# Bash recipe for building across all platforms
script = raw"""
# Export dependencies
mkdir ${prefix}/deps
cd ${libdir}
for file in $(ls .); do
   if [[ -f $file ]]; then
      if [[ -z $(ls -la $file | grep 'artifacts') ]]; then
         cp -P ${file} ${prefix}/deps/${file}
      else
         cp -L ${file} ${prefix}/deps/${file}
      fi
   fi
done
cd ${prefix}
cp -rL share/licenses deps/licenses
chmod -R u=rwx deps
tar -czvf deps.tar.gz deps
rm -r deps

# Update Ninja
cp ${host_prefix}/bin/ninja /usr/bin/ninja

cd ${WORKSPACE}/srcdir/SIFDecode
meson setup builddir --cross-file=${MESON_TARGET_TOOLCHAIN%.*}_gcc.meson --prefix=$prefix
meson compile -C builddir
meson install -C builddir
"""

# These are the platforms we will build for by default, unless further
# platforms are passed in on the command line
platforms = supported_platforms()
platforms = expand_gfortran_versions(platforms)

# The products that we will ensure are always built
products = [
    LibraryProduct("libsifdecode", :libsifdecode)
]

# Dependencies that must be installed before this package can be built
dependencies = [
    HostBuildDependency(PackageSpec(name="Ninja_jll", uuid="76642167-d241-5cee-8c94-7a494e8cb7b7")),
    Dependency(PackageSpec(name="CompilerSupportLibraries_jll", uuid="e66e0078-7015-5450-92f7-15fbd957f2ae")),
]

# Build the tarballs, and possibly a `build.jl` as well.
build_tarballs(ARGS, name, version, sources, script, platforms, products, dependencies; preferred_gcc_version=v"9.1", julia_compat="1.6")
