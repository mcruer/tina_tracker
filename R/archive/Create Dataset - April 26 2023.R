
#Read in data from files and prep it ----

#load("saved.RData")
library(DataEDU)
library (CProg)
library(magrittr)


#board # 22


key <- mcp.2022.sep %>%
  filter_out(construction.status, "cancelled") %>%
  filter (opening.date > as_date("2022-09-01")) %>%
  select (project.id)

#create Key to identify possible missing data ----

# key <- data.list %>%
#   reduce(full_join) %>%  
#   filter_out_na(project.id) %>%
#   set_names (c("project.id", "cp", "lp", "cc", "eo")) %>%
#   mutate_a(-project.id, .f=~if_else(is.na(.x), FALSE, TRUE)) %>%
#   mutate(new = if_else(cp+lp+cc+eo == 0, TRUE, FALSE)) %>%
#   left_join(pt %>%
#               select (project.id) %>%
#               mutate (pt = TRUE)) %>%
#   left_join(mcp.previous %>%
#               select (project.id) %>%
#               mutate(mcp.previous = TRUE)) %>%
#   mutate_a(~if_else(is.na(.x)|.x==FALSE, NA, .x), -project.id) 


#Create MCP New ----

# id.fix <- read_excel("key - with connecting IDs.xlsx") %>%
#   filter_out_na(connecting.id) %>%
#   select (ends_with("id"))

#data.list[[1]]

previous.mcp <- mcp.2022.sep

mcp.new <- previous.mcp %>%
  filter_out(construction.status, "cancelled") %>%
  filter (opening.date > as_date("2022-09-01")|str_detect(project.id, "09-023-01|30.1-013|5.2-010|5.2-013")) %>%
  select (project.id:completion.date) %>%
  select (-atp.date) %>%
  filter_out_na(board.number) %>% 
  left_join (pt %>%
               select (
                 project.id, #PT
                 atp.date, #PT?
                 lp.funding, #PT
                 cp.funding, #PT
                 fdk.funding, #PT
                 cc.funding, #PT
                 eo.funding, #PT
                 chr.funding, #PT
                 total.funding, #PT needed to calculate "other.funding"
                 
               )) %>%  
  mutate(
    other.funding = total.funding - (
      +lp.funding
      + cp.funding
      + fdk.funding
      + cc.funding
      + eo.funding
      + chr.funding
    ),
    scope.change = NA_character_,
    site.ownership.status = NA_character_,
    scope.sqft.new = NA_integer_,
    scope.sqft.retro = NA_integer_,
    design.status = NA_character_,
    childcare.floorplan.approved = NA_character_,
    site.plan.approved = NA_character_,
    design.comments = NA_character_,
    site.comments = NA_character_,
    site.purchase.date = NA,
    cost.estimate.basis = NA_character_,
    total.funding = "calculation",
    total.cost = "calculation",
    variance.cost = "calculation",
    land.cost = NA_integer_, #Previous MCP?
    building.cost = NA_integer_, #Previous MCP?
    fte.cost = NA_integer_, #Previous MCP?
    cost.comments = NA_character_,
    comments = NA_character_,
  ) %>% 
  select (
    project.id, #1 PT
    project.name.long, #2 MCP
    board.number, #3 MCP
    board.name, #4 MCP
    project.type, #5 MCP
    project.catigory, #6 MCP    
    ori.sy, #7 PT
    construction.status, #8 Previous MCP
    scope.change, #9
    
    scope.sqft.new, #10
    scope.sqft.retro, #11
    design.status, #12
    childcare.floorplan.approved, #13
    design.comments, #14
      site.ownership.status, #15
     site.plan.approved, #16
    site.purchase.date, #17
    site.comments, #18
    atp.anticipated.date, #19 Previous MCP?
    atp.date, # 20 PT
    construction.date, #21 Previous MCP?
    substantial.completion.date, #22 Previous MCP?
    opening.date, #23 Previous MCP?
    completion.date, #24 Previous MCP?
    lp.funding, #25 PT
    cp.funding, #26 PT
    fdk.funding, #27 PT
    cc.funding, #28 PT
    eo.funding, #29 PT
    chr.funding, #30 PT
    other.funding, #31 PT
    total.funding, #32 Calculation
    cost.estimate.basis, #33
    land.cost, #34 Previous MCP?
    building.cost, #35 Previous MCP?
    fte.cost, #36 Previous MCP?
    total.cost, #37 Calculation
    variance.cost, #38 Calculation
    cost.comments, #39
    comments #40
  ) %>%
  parse_guess_all() %>%
  arrange(project.id) %>% 
  #parse_guess_all() %>%
  bind_rows(pt %>% 
              filter_pid("60.1-028") %>%
              mutate (other.funding = total.funding) %>%
              reclass_c(total.funding)) %>%
  select(project.id:comments) %>%
  view

col.names <- mcp.new %>% names

save(col.names, file = "data/col.names.RData")



