.is_redirected <- function(url)
{
    assert_string(url)

    connection <- HEAD(url)
    result <- connection$url != url

    return(result)
}
