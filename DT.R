library(data.table)
library(visNetwork)

cases <- data.table::fread("MOCK_DATA.csv") 
na.omit(cases, cols = 1:2)

sources <- setnames(cases[,unique(.SD), .SDcols = "id1"], old = "id1", new = "label")

contacts <- setnames(cases[,unique(.SD), .SDcols = "id2"],old = "id2", new = "label")

nodes <- merge(contacts, sources, by = "label", all = TRUE) ## full outer join
nodes[,id := rleid(label)]

per_case <- cases[, weight := .N,by = c("id1","id2")]

edges <- setnames(nodes[per_case, on = c("label" = "id1")],"id","from")

edges <- setnames(nodes[edges, on = c("label" = "id2")], "id","to")[]
edges[,!c("label","i.label")][,.(from,to,weight)][]

# contacts who are also sources

sourcesvec <- sources[,(label)]

contactsvec <- contacts[,(label)]

related <- which(sourcesvec %chin% contactsvec)

sources_id <- sources[,"label"][,record_type := 'index']

related_id <- sources[related,][,record_type := 'index_and_contact']

index_lookup <- rbind(sources_id, related_id)

nodes <- index_lookup[nodes, on = "label"][]

nodes[is.na(record_type), record_type := 'contact_only'][]

nodes[,`:=`(shadow = TRUE, title = label) ]
nodes$shadow <- TRUE # Nodes will  have a drop shadow
nodes$title  <- nodes$label # Text on click
#nodes$label  <- nodes$label # Node label

# use fcase
nodes[,color.background := fcase(
    record_type == 'index', "firebrick",
    record_type == 'index_and_contact', "gold",
    default = "steelblue"
)]

nodes[,`:=`(color.border = "black",
            color.highlight.background = "orange",
            color.highlight.border = "darkred")]



#some nodes are both index, and index and contact
# we need to dedupe them, so we get rid of the index

nodes[,idcount := .N, by = id][]

dupes <- nodes[idcount == 2 & record_type == "index",][]
nodes <- fsetdiff(nodes,dupes)
setnames(nodes[,idcount := NULL],"record_type", "group")

edges[,`:=`(
    color = "gray",    # line color
    arrows = "middle", # arrows: 'from', 'to', or 'middle'
    smooth = FALSE ,   # should the edges be curved?
    shadow = FALSE    # edge shadow  
    )]


visnet <- visNetwork(nodes, edges) %>%
    visGroups(groupname = "index", color = "firebrick", shape = "diamond") %>%
    visGroups(groupname = "index_and_contact", color = "gold", shape = "triangle") %>%
    visGroups(groupname = "contact_only", color =  "steelblue", shape = "circle") %>%
    visLegend(position = "right", main = "Sample index and contact")

visnet
