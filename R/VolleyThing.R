VolleyThing <- R6Class(

classname = "VolleyThing",

private = list(
    # Private fields -----------------------------------------------------------
    .urls = list(),
    .xmls = list(),

    # Private methods ----------------------------------------------------------
    .get_xmls = function(name, check_response = FALSE)
    {
        xmls <- private$.xmls[[name]]
        if (!is.null(xmls)) {
            return(xmls)
        }

        urls <- private$.urls[[name]]

        if (check_response) {
            redir <- sapply(urls, .is_redirected)
            no_redir <- !redir

            xmls <- vector(mode = "list", length = length(urls))
            xmls[redir] <- lapply(urls[redir],
                                  function(x) as_xml_document(list()))
            xmls[no_redir] <- lapply(urls[no_redir], read_html)
        } else {
            xmls <- lapply(urls, read_html)
        }

        private$.xmls[[name]] <- xmls
        return(xmls)

    }
),

public = list(
    # Public fields ------------------------------------------------------------
    seasons = NULL,
    dt = NULL,

    # Public methods -----------------------------------------------------------
    fetch = function(what)
    {
        assert(check_string(what), check_subset(what, private$.fetch_options),
               combine = "and")

        fun_name <- paste0(".fetch_", what)
        result <- private[[fun_name]]()
        return(result)
    }
)

# End of class definition
)
