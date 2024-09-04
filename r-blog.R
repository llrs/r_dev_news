# Script to toot blog
library("rtoot")
url_rss <- "https://blog.r-project.org/index.xml"

xml <- read_xml(url_rss, options = c("RECOVER", "NOERROR", "NOBLANKS"))
publication <- xml_find_first(xml, ".//item")
list_publication <- vector("list", xml_length(publication))
details <- xml_children(publication)

names(list_publication) <- xml_name(details)
list_publication[] <- xml_text(details)
locale <- Sys.setlocale(category = "LC_ALL", locale = "C")
Sys.setLanguage("en")
date_publication <- as.Date(strptime(list_publication$pubDate,
                                     format = "%a, %d %b %Y %X %z",
                                     tz = "Europe/Madrid"))
date_now <- Sys.Date()
fi <- file.mtime("r-blog.log")
d <- as.Date(fi, tz = Sys.timezone())
published_today <- date_publication >= date_now & d <= date_now

if (!published_today) {
  q(save = "no")
}

url_length <- 25
toot_length_max <- 500
# One more for the newline
usable_length <- toot_length_max - url_length - 1

header <- paste0("The R blog: ", list_publication$title, "\n")
message <- paste0(header, trimws(list_publication$description))
if (nchar(message) > usable_length) {
  message <- substring(message, 1, usable_length - 3)
  message <- paste0(message, "...\n", list_publication$link)
} else {
  message <- paste0(message, "\n", list_publication$link)
}

token_file <- file.path(tools::R_user_dir("rtoot", "config"), "R_devs_news.rds")
if (!file.exists(token_file)) {
  stop("Missing credentials to post as the bot")
}
token <- readRDS(token_file)
post_toot(status = message, language = "en", token = token)
