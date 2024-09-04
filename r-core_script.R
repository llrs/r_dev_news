# Script to be run as from r-devel.R or with any other tag
date <- Sys.Date()
message("Starting the script for ", date)
url_feed <- sprintf("https://developer.r-project.org/blosxom.cgi/%s/NEWS/", tag)
url_rss <- paste0(url_feed, "index.rss")
# TODO - handle failure
xml <- read_xml(url_rss, options = c("RECOVER", "NOERROR", "NOBLANKS"))
items <- xml_find_first(xml, ".//item")
text <- xml_find_first(items, "description") |> xml_text()
date <- xml_find_first(items, ".//title") |> xml_text()
date <- chartr("-", "/", date)
file_info <- paste0("info_", tag, ".rds")
if (file.exists(file_info)) {
  messages_not_posted <- readRDS(file_info)
} else {
  messages_not_posted <- NULL
}

extract_text <- function(text) {
  html <- read_html(text)
  titles <- xml_find_all(html, "//h4") |> xml_text() |> trimws()

  # Split for each point
  ul <- xml_find_all(html, "//ul")
  n_elements <- sapply(ul, function(x){length(xml_children(x))})
  # Remove elements that are deleted/moved to other sections:
  dels <- html |>
    xml_find_all("./body/ul/li//del") |>
    xml_text() |>
    trimws()
  changes <- xml_find_all(ul, "//li") |>
    xml_text() |>
    trimws()

  if (length(dels) > 1) {
    warning("More than one deleted news piece: FIXME")
  }

  # Text that moved
  rem <- match(dels, changes)
  if (length(rem) >= 1) {
    pos <- which(!cumsum(n_elements) < rem)[1]
    n_elements[pos] <- n_elements[pos] - 1
    if (sum(n_elements) == 0) {
      return(NULL)
    }
  } else {
    # Avoid removing existing changes/NEWS
    rem <- length(changes) + 1
  }

  reps <- rep.int(seq_along(n_elements), times = n_elements)

  changes2 <- changes[-rem] |>
    gsub(pattern = "\\n", replacement = " ") |>
    gsub(pattern = "\\s+", replacement = " ")
  s <- split(changes2, reps)
  names(s) <- titles[n_elements != 0]
  s
}

clean_text <- extract_text(text)
# Prevent messaging about previous versions of R
# Sometimes there are "CHANGES IN R 4.0.0" when it was released some years ago.
# Post only those from the given tag (excep the branch)
if (tag != "R-devel") {
  type <- gsub("-branch", "", tag) |>
    gsub(pattern = "R-", replacement = "R ", fixed = TRUE) |>
    gsub(pattern = "-", replacement = ".", fixed = TRUE)
} else {
  type <- tag
}
latest_changes <- grepl(type, names(clean_text))
clean_text <- clean_text[latest_changes]

url_length <- 25
toot_length_max <- 500
usable_length <- toot_length_max - url_length - 1 # for new line

trim_message <- function(message, url, max_length) {
  if (nchar(message) > max_length) {
    message <- substring(message, 1, max_length - 3)
    message <- paste0(message, "...\n", url)
  } else {
    message <- paste0(message, "\n", url)
  }
  message
}

# Prepare message ####
prepare_messages <- function(x) {
  if (!is.list(x)) {
    header <- paste0(names(x), ":\n")
    url_note <- paste0(url_feed, date)
    xy <- paste0(header, trimws(x))
    return(trim_message(xy, url_note, usable_length))
  }
  u <- unlist(x, use.names = FALSE)
  names(u) <- rep(names(x), lengths(x))
  for (i in seq_along(u)) {
    header <- paste0(names(u)[i], ":\n")
    url_note <- paste0(url_feed, date)
    xy <- paste0(header, trimws(u[i]))
    u[i] <- trim_message(xy, url_note, usable_length)
  }
  u
}
saveRDS(clean_text, file = file_info)

messages_not_posted <- setdiff(clean_text, messages_not_posted)
lengths_messages <- lengths(messages_not_posted)
if (sum(lengths_messages) > 7) {
  lengths_messages[lengths_messages > 1L] <- paste0(lengths_messages[lengths_messages > 1L], " elements")
  lengths_messages[lengths_messages == 1L] <- messages_not_posted[lengths_messages == 1L]
  messages_not_posted <- lengths_messages
} else if (sum(lengths_messages, na.rm = TRUE) == 0) {
  quit(save = "no")
}


messages_ready <- prepare_messages(messages_not_posted)
token_file <- file.path(tools::R_user_dir("rtoot", "config"), "R_devs_news.rds")
if (!file.exists(token_file)) {
  stop("Missing credentials to post as the bot")
}
token <- readRDS(token_file)
a <- sapply(messages_ready, function(x){
  post_toot(status = x, language = "en", token = token)
  Sys.sleep(0.1)
})
#rutils::llrs_send_ntfy("Successfully ran r-devel cron job", title = "CRON")
message("Successfully ran the script for ", date)
