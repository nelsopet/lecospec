base_path <- "./Functions/"

code_path <- paste0(base_path, "lecospectR.R")
export_path <- base_path
package_path <- paste0(export_path, "lecospectR/")

package.skeleton(
    name = "lecospectR",
    path = export_path,
    code_files = c(code_path),
    encoding = "UTF-8"
)
# Remove old documentation files created by package.skeleton
man_files <- list.files(paste0(export_path, "lecospectR/man/"), full.names = TRUE )
map(man_files, file.remove)
# remove the old namespace file, too.
file.remove(
    paste0(
        export_path,
        "lecospectR/NAMESPACE"
    )
)

roxygen2::roxygenise(
    package_path,
    clean = TRUE
)

devtools::load_all(
    package_path
)


