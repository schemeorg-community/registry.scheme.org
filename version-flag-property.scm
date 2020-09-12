(id build.configure)
(description "Command line arguments to configure script")
(type "string ...")

(id build.date)
(description "Date (and time) of build")
(type "iso-date-string")

(id build.git.branch)
(description "Git repository state at build time - branch")
(type "string")

(id build.git.commit)
(description "Git repository state at build time - commit hash")
(type "string")

(id build.git.modified)
(description "Git repository state at build time - dirty files")
(type "string ...")

(id build.git.tag)
(description "Git repository state at build time - tag")
(type "string")

(id build.platform)
(description "OS/machine/etc. for which build was made")
(type "string")

(id c.compile)
(description "C programming language compiler command")
(type "string ...")

(id c.link)
(description "C programming language linker command")
(type "string ...")

(id c.type-bits)
(description "C programming language data type sizes")
(type "(symbol integer) ...")

(id c.version)
(description "C programming language compiler version")
(type "string")

(id command)
(description "Canonical shell command name for this program")
(type "string")

(id encodings)
(description "Character encodings supported by the implementation, default first")
(type "symbol ...")

(id image.date)
(description "Date (and time) the Scheme image was saved")
(type "iso-date-string")

(id image.file)
(description "File the Scheme image is loaded from")
(type "string")

(id install-dir)
(description "Installation prefix directory (e.g. /usr/local)")
(type "string")

(id jvm.*)
(description "Java system properties")
(type "string")

(id languages)
(description "Programming languages supported by the implementation")
(type "symbol ...")

(id os.env.*)
(description "Runtime environment variables")
(type "string")

(id os.stdio)
(description "Stdin, stdout, and stderr file types")
(type "symbol symbol symbol")

(id os.uname)
(description "Runtime Unix uname values")
(type "string string string")

(id release)
(description "Last release version number")
(type "string")

(id release.date)
(description "Last release date")
(type "iso-date-string")

(id release.name)
(description "Last release codename")
(type "string")

(id scheme.features)
(description "List of cond-expand features present")
(type "symbol ...")

(id scheme.id)
(description "The scheme-id of this implementation")
(type "symbol")

(id scheme.path)
(description "Directories to search for Scheme libraries")
(type "string ...")

(id scheme.srfi)
(description "Supported SRFIs (list may not be exhaustive)")
(type "integer ...")

(id website)
(description "URL of the implementation's website")
(type "string")
