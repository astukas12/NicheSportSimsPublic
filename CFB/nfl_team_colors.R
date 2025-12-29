# NFL Team Colors
# Golden Ticket Sims - NFL Edition
# Returns primary and secondary colors for NFL teams

get_nfl_team_color <- function(team_name, type = "primary") {
  # Normalize team name for matching
  team_lower <- tolower(trimws(team_name))
  
  # NFL Team Colors (Primary and Secondary)
  nfl_colors <- list(
    # AFC East
    "buffalo bills" = list(primary = "#00338D", secondary = "#C60C30"),
    "bills" = list(primary = "#00338D", secondary = "#C60C30"),
    "buf" = list(primary = "#00338D", secondary = "#C60C30"),
    
    "miami dolphins" = list(primary = "#008E97", secondary = "#FC4C02"),
    "dolphins" = list(primary = "#008E97", secondary = "#FC4C02"),
    "mia" = list(primary = "#008E97", secondary = "#FC4C02"),
    
    "new england patriots" = list(primary = "#002244", secondary = "#C60C30"),
    "patriots" = list(primary = "#002244", secondary = "#C60C30"),
    "ne" = list(primary = "#002244", secondary = "#C60C30"),
    
    "new york jets" = list(primary = "#125740", secondary = "#000000"),
    "jets" = list(primary = "#125740", secondary = "#000000"),
    "nyj" = list(primary = "#125740", secondary = "#000000"),
    
    # AFC North
    "baltimore ravens" = list(primary = "#241773", secondary = "#000000"),
    "ravens" = list(primary = "#241773", secondary = "#000000"),
    "bal" = list(primary = "#241773", secondary = "#000000"),
    
    "cincinnati bengals" = list(primary = "#FB4F14", secondary = "#000000"),
    "bengals" = list(primary = "#FB4F14", secondary = "#000000"),
    "cin" = list(primary = "#FB4F14", secondary = "#000000"),
    
    "cleveland browns" = list(primary = "#311D00", secondary = "#FF3C00"),
    "browns" = list(primary = "#311D00", secondary = "#FF3C00"),
    "cle" = list(primary = "#311D00", secondary = "#FF3C00"),
    
    "pittsburgh steelers" = list(primary = "#FFB612", secondary = "#000000"),
    "steelers" = list(primary = "#FFB612", secondary = "#000000"),
    "pit" = list(primary = "#FFB612", secondary = "#000000"),
    
    # AFC South
    "houston texans" = list(primary = "#03202F", secondary = "#A71930"),
    "texans" = list(primary = "#03202F", secondary = "#A71930"),
    "hou" = list(primary = "#03202F", secondary = "#A71930"),
    
    "indianapolis colts" = list(primary = "#002C5F", secondary = "#A2AAAD"),
    "colts" = list(primary = "#002C5F", secondary = "#A2AAAD"),
    "ind" = list(primary = "#002C5F", secondary = "#A2AAAD"),
    
    "jacksonville jaguars" = list(primary = "#006778", secondary = "#D7A22A"),
    "jaguars" = list(primary = "#006778", secondary = "#D7A22A"),
    "jax" = list(primary = "#006778", secondary = "#D7A22A"),
    
    "tennessee titans" = list(primary = "#0C2340", secondary = "#4B92DB"),
    "titans" = list(primary = "#0C2340", secondary = "#4B92DB"),
    "ten" = list(primary = "#0C2340", secondary = "#4B92DB"),
    
    # AFC West
    "denver broncos" = list(primary = "#FB4F14", secondary = "#002244"),
    "broncos" = list(primary = "#FB4F14", secondary = "#002244"),
    "den" = list(primary = "#FB4F14", secondary = "#002244"),
    
    "kansas city chiefs" = list(primary = "#E31837", secondary = "#FFB81C"),
    "chiefs" = list(primary = "#E31837", secondary = "#FFB81C"),
    "kc" = list(primary = "#E31837", secondary = "#FFB81C"),
    
    "las vegas raiders" = list(primary = "#000000", secondary = "#A5ACAF"),
    "raiders" = list(primary = "#000000", secondary = "#A5ACAF"),
    "lv" = list(primary = "#000000", secondary = "#A5ACAF"),
    
    "los angeles chargers" = list(primary = "#0080C6", secondary = "#FFC20E"),
    "chargers" = list(primary = "#0080C6", secondary = "#FFC20E"),
    "lac" = list(primary = "#0080C6", secondary = "#FFC20E"),
    
    # NFC East
    "dallas cowboys" = list(primary = "#041E42", secondary = "#869397"),
    "cowboys" = list(primary = "#041E42", secondary = "#869397"),
    "dal" = list(primary = "#041E42", secondary = "#869397"),
    
    "new york giants" = list(primary = "#0B2265", secondary = "#A71930"),
    "giants" = list(primary = "#0B2265", secondary = "#A71930"),
    "nyg" = list(primary = "#0B2265", secondary = "#A71930"),
    
    "philadelphia eagles" = list(primary = "#004C54", secondary = "#A5ACAF"),
    "eagles" = list(primary = "#004C54", secondary = "#A5ACAF"),
    "phi" = list(primary = "#004C54", secondary = "#A5ACAF"),
    
    "washington commanders" = list(primary = "#5A1414", secondary = "#FFB612"),
    "commanders" = list(primary = "#5A1414", secondary = "#FFB612"),
    "was" = list(primary = "#5A1414", secondary = "#FFB612"),
    
    # NFC North
    "chicago bears" = list(primary = "#0B162A", secondary = "#C83803"),
    "bears" = list(primary = "#0B162A", secondary = "#C83803"),
    "chi" = list(primary = "#0B162A", secondary = "#C83803"),
    
    "detroit lions" = list(primary = "#0076B6", secondary = "#B0B7BC"),
    "lions" = list(primary = "#0076B6", secondary = "#B0B7BC"),
    "det" = list(primary = "#0076B6", secondary = "#B0B7BC"),
    
    "green bay packers" = list(primary = "#203731", secondary = "#FFB612"),
    "packers" = list(primary = "#203731", secondary = "#FFB612"),
    "gb" = list(primary = "#203731", secondary = "#FFB612"),
    
    "minnesota vikings" = list(primary = "#4F2683", secondary = "#FFC62F"),
    "vikings" = list(primary = "#4F2683", secondary = "#FFC62F"),
    "min" = list(primary = "#4F2683", secondary = "#FFC62F"),
    
    # NFC South
    "atlanta falcons" = list(primary = "#A71930", secondary = "#000000"),
    "falcons" = list(primary = "#A71930", secondary = "#000000"),
    "atl" = list(primary = "#A71930", secondary = "#000000"),
    
    "carolina panthers" = list(primary = "#0085CA", secondary = "#000000"),
    "panthers" = list(primary = "#0085CA", secondary = "#000000"),
    "car" = list(primary = "#0085CA", secondary = "#000000"),
    
    "new orleans saints" = list(primary = "#D3BC8D", secondary = "#000000"),
    "saints" = list(primary = "#D3BC8D", secondary = "#000000"),
    "no" = list(primary = "#D3BC8D", secondary = "#000000"),
    
    "tampa bay buccaneers" = list(primary = "#D50A0A", secondary = "#34302B"),
    "buccaneers" = list(primary = "#D50A0A", secondary = "#34302B"),
    "tb" = list(primary = "#D50A0A", secondary = "#34302B"),
    
    # NFC West
    "arizona cardinals" = list(primary = "#97233F", secondary = "#000000"),
    "cardinals" = list(primary = "#97233F", secondary = "#000000"),
    "ari" = list(primary = "#97233F", secondary = "#000000"),
    
    "los angeles rams" = list(primary = "#003594", secondary = "#FFA300"),
    "rams" = list(primary = "#003594", secondary = "#FFA300"),
    "la" = list(primary = "#003594", secondary = "#FFA300"),
    "lar" = list(primary = "#003594", secondary = "#FFA300"),
    
    "san francisco 49ers" = list(primary = "#AA0000", secondary = "#B3995D"),
    "49ers" = list(primary = "#AA0000", secondary = "#B3995D"),
    "sf" = list(primary = "#AA0000", secondary = "#B3995D"),
    
    "seattle seahawks" = list(primary = "#002244", secondary = "#69BE28"),
    "seahawks" = list(primary = "#002244", secondary = "#69BE28"),
    "sea" = list(primary = "#002244", secondary = "#69BE28")
  )
  
  # Try to find team in the list
  if (team_lower %in% names(nfl_colors)) {
    colors <- nfl_colors[[team_lower]]
    if (type == "secondary") {
      return(colors$secondary)
    } else {
      return(colors$primary)
    }
  }
  
  # Default fallback color if team not found
  if (type == "secondary") {
    return("#999999")
  } else {
    return("#003366")
  }
}

# Wrapper function for backwards compatibility
get_nfl_colors <- function(team_name, type = "primary") {
  return(get_nfl_team_color(team_name, type))
}
