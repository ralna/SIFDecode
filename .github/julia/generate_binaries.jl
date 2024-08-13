# Version
haskey(ENV, "SIFDECODE_RELEASE") || error("The environment variable SIFDECODE_RELEASE is not defined.")
version = VersionNumber(ENV["SIFDECODE_RELEASE"])
version2 = ENV["SIFDECODE_RELEASE"]
package = "SIFDecode"

platforms = [
   ("aarch64-apple-darwin-libgfortran5"  , "lib", "dylib"),
#  ("aarch64-linux-gnu-libgfortran3"     , "lib", "so"   ),
#  ("aarch64-linux-gnu-libgfortran4"     , "lib", "so"   ),
#  ("aarch64-linux-gnu-libgfortran5"     , "lib", "so"   ),
#  ("aarch64-linux-musl-libgfortran3"    , "lib", "so"   ),
#  ("aarch64-linux-musl-libgfortran4"    , "lib", "so"   ),
#  ("aarch64-linux-musl-libgfortran5"    , "lib", "so"   ),
#  ("powerpc64le-linux-gnu-libgfortran3" , "lib", "so"   ),
#  ("powerpc64le-linux-gnu-libgfortran4" , "lib", "so"   ),
#  ("powerpc64le-linux-gnu-libgfortran5" , "lib", "so"   ),
#  ("x86_64-apple-darwin-libgfortran3"   , "lib", "dylib"),
#  ("x86_64-apple-darwin-libgfortran4"   , "lib", "dylib"),
   ("x86_64-apple-darwin-libgfortran5"   , "lib", "dylib"),
#  ("x86_64-linux-gnu-libgfortran3"      , "lib", "so"   ),
#  ("x86_64-linux-gnu-libgfortran4"      , "lib", "so"   ),
   ("x86_64-linux-gnu-libgfortran5"      , "lib", "so"   ),
#  ("x86_64-linux-musl-libgfortran3"     , "lib", "so"   ),
#  ("x86_64-linux-musl-libgfortran4"     , "lib", "so"   ),
#  ("x86_64-linux-musl-libgfortran5"     , "lib", "so"   ),
#  ("x86_64-unknown-freebsd-libgfortran3", "lib", "so"   ),
#  ("x86_64-unknown-freebsd-libgfortran4", "lib", "so"   ),
#  ("x86_64-unknown-freebsd-libgfortran5", "lib", "so"   ),
#  ("x86_64-w64-mingw32-libgfortran3"    , "bin", "dll"  ),
#  ("x86_64-w64-mingw32-libgfortran4"    , "bin", "dll"  ),
   ("x86_64-w64-mingw32-libgfortran5"    , "bin", "dll"  ),
]


for (platform, libdir, ext) in platforms

  tarball_name = "$package.v$version.$platform.tar.gz"

  if isfile("products/$(tarball_name)")
    # Unzip the tarball generated by BinaryBuilder.jl
    isdir("products/$platform") && rm("products/$platform", recursive=true)
    mkdir("products/$platform")
    run(`tar -xzf products/$(tarball_name) -C products/$platform`)

    if isfile("products/$platform/deps.tar.gz")
      # Unzip the tarball of the dependencies
      run(`tar -xzf products/$platform/deps.tar.gz -C products/$platform`)

      # Copy the license of each dependency
      for folder in readdir("products/$platform/deps/licenses")
        cp("products/$platform/deps/licenses/$folder", "products/$platform/share/licenses/$folder")
      end
      rm("products/$platform/deps/licenses", recursive=true)

      # Copy the shared library of each dependency
      for file in readdir("products/$platform/deps")
        cp("products/$platform/deps/$file", "products/$platform/$libdir/$file")
      end

      # Remove the folder used to unzip the tarball of the dependencies
      rm("products/$platform/deps", recursive=true)
      rm("products/$platform/deps.tar.gz", recursive=true)

      # Create the archives *_binaries
      isfile("$(package)_binaries.$version2.$platform.tar.gz") && rm("$(package)_binaries.$version2.$platform.tar.gz")
      isfile("$(package)_binaries.$version2.$platform.zip") && rm("$(package)_binaries.$version2.$platform.zip")
      cd("products/$platform")

      # Create a folder with the version number of the package
      mkdir("$(package)_binaries.$version2")
      for folder in ("share", "modules", "lib", "bin")
        cp(folder, "$(package)_binaries.$version2/$folder")
      end

      cd("$(package)_binaries.$version2")
      if ext == "dll"
        run(`zip -r --symlinks ../../../$(package)_binaries.$version2.$platform.zip share modules lib bin`)
      else
        run(`tar -czf ../../../$(package)_binaries.$version2.$platform.tar.gz share modules lib bin`)
      end
      cd("../../..")

      # Remove the folder used to unzip the tarball generated by BinaryBuilder.jl
      rm("products/$platform", recursive=true)
    else
      @warn("The tarball deps.tar.gz is missing in $(tarball_name)!")
    end
  else
    @warn("The tarball for the platform $platform was not generated!")
  end
end
