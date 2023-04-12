.onAttach <- function(lib, pkg) {
    packageStartupMessage(
        paste(
            "R-Ecopol version",
            utils::packageDescription("REcopol",
                fields = "Version"
            ),
            "\n"
        ),
        appendLF = TRUE
    )
}