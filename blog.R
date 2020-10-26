
2020-10-25-network-plots.md
================

## Contact tracing example



library(data.table)
library(dplyr)
library(tibble)
library(visNetwork)

``` r
cases <- data.table::fread("MOCK_DATA.csv") %>% 
    filter(complete.cases(.)) 

head(cases)
```

    ##            id1         id2
    ## 1: 536-10-8682 188-42-4468
    ## 2: 280-30-2349 750-47-9722
    ## 3: 214-05-0872 412-54-2941
    ## 4: 712-65-9980 515-04-9079
    ## 5: 356-34-3345 599-84-5728
    ## 6: 164-38-1022 801-50-4484

## Distinct index cases and contacts

``` r
sources <- cases %>%
    distinct(id1) %>%
    rename(label = id1)

contacts <- cases %>%
    distinct(id2) %>%
    rename(label = id2)
```

``` r
nodes <- full_join(sources, contacts, by = c("label"))
nodes <- nodes %>% rowid_to_column("id")

head(nodes)
```

    ##    id       label
    ## 1:  1 536-10-8682
    ## 2:  2 280-30-2349
    ## 3:  3 214-05-0872
    ## 4:  4 712-65-9980
    ## 5:  5 356-34-3345
    ## 6:  6 164-38-1022

``` r
per_case <- cases %>%
    group_by(id1, id2) %>%
    summarise(weight = n()) %>%
    ungroup()
```

    ## `summarise()` regrouping output by 'id1' (override with `.groups` argument)

``` r
head(per_case,10)
```

    ## # A tibble: 10 x 3
    ##    id1         id2         weight
    ##    <chr>       <chr>        <int>
    ##  1 164-38-1022 177-96-6142      1
    ##  2 164-38-1022 295-98-8195      1
    ##  3 164-38-1022 801-50-4484      1
    ##  4 180-90-3238 407-90-2819      1
    ##  5 180-90-3238 595-05-8736      1
    ##  6 188-07-5552 620-43-5564      1
    ##  7 201-94-4265 189-87-2804      1
    ##  8 201-94-4265 536-10-8682      1
    ##  9 201-94-4265 558-62-5961      1
    ## 10 214-05-0872 412-54-2941      1

``` r
edges <- per_case %>%
    left_join(nodes, by = c("id1" = "label")) %>%
    rename(from = id)

head(edges,5)
```

    ## # A tibble: 5 x 4
    ##   id1         id2         weight  from
    ##   <chr>       <chr>        <int> <int>
    ## 1 164-38-1022 177-96-6142      1     6
    ## 2 164-38-1022 295-98-8195      1     6
    ## 3 164-38-1022 801-50-4484      1     6
    ## 4 180-90-3238 407-90-2819      1    15
    ## 5 180-90-3238 595-05-8736      1    15

``` r
edges <- edges %>%
    left_join(nodes, by = c("id2" = "label")) %>%
    rename(to = id)

head(edges,5)
```

    ## # A tibble: 5 x 5
    ##   id1         id2         weight  from    to
    ##   <chr>       <chr>        <int> <int> <int>
    ## 1 164-38-1022 177-96-6142      1     6    41
    ## 2 164-38-1022 295-98-8195      1     6    45
    ## 3 164-38-1022 801-50-4484      1     6    25
    ## 4 180-90-3238 407-90-2819      1    15    47
    ## 5 180-90-3238 595-05-8736      1    15    34

``` r
edges <- select(edges, from, to, weight)
head(edges,5)
```

    ## # A tibble: 5 x 3
    ##    from    to weight
    ##   <int> <int>  <int>
    ## 1     6    41      1
    ## 2     6    45      1
    ## 3     6    25      1
    ## 4    15    47      1
    ## 5    15    34      1

``` r
nodes[id == 6,]
```

    ##    id       label
    ## 1:  6 164-38-1022

## contacts and sources

``` r
sourcesvec <- sources$label

contactsvec <- contacts$label

related <- which(sourcesvec %in% contactsvec)

sources_id <- sourcesvec %>%
    as_tibble_col(.,column_name = 'label') %>%
    mutate(record_type = "index")

related_id <- sources[related,] %>%
   # as_tibble_col(., column_name = 'label') %>%
    mutate(record_type = "index_and_contact")
```

``` r
index_lookup <- bind_rows(sources_id,related_id)
```

``` r
nodes <- nodes %>%
    left_join(index_lookup, by = "label")

head(nodes, 5)
```

    ##    id       label       record_type
    ## 1:  1 536-10-8682             index
    ## 2:  1 536-10-8682 index_and_contact
    ## 3:  2 280-30-2349             index
    ## 4:  3 214-05-0872             index
    ## 5:  4 712-65-9980             index

``` r
nodes <- nodes %>%
    mutate(record_type = case_when(!is.na(record_type) ~ record_type,
                                   TRUE ~ 'contact_only'))
head(nodes, 5)
```

    ##    id       label       record_type
    ## 1:  1 536-10-8682             index
    ## 2:  1 536-10-8682 index_and_contact
    ## 3:  2 280-30-2349             index
    ## 4:  3 214-05-0872             index
    ## 5:  4 712-65-9980             index

``` r
nodes$shadow <- TRUE # Nodes will  have a drop shadow
nodes$title  <- nodes$label # Text on click
nodes$label  <- nodes$label # Node label

nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
```

``` r
#some nodes are both index, and index and contact
# we need to dedupe them, so we get rid of the index

dupes <- nodes %>%
    group_by(id) %>%
    tally() %>%
    filter(n >= 2) %>%
    pull(id)
```

``` r
nodes_to_remove <- nodes %>%
    filter(id %in% dupes &  record_type == 'index')
```

``` r
nodes <- anti_join(nodes, nodes_to_remove)
```

    ## Joining, by = c("id", "label", "record_type", "shadow", "title", "color.border", "color.highlight.background", "color.highlight.border")

``` r
edges$color <- "gray"    # line color
edges$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
edges$smooth <- FALSE    # should the edges be curved?
edges$shadow <- FALSE    # edge shadow
```

``` r
nodes <- nodes %>% rename(group = record_type)
head(nodes, 5)
```

    ##    id       label             group shadow       title color.border
    ## 1:  1 536-10-8682 index_and_contact   TRUE 536-10-8682        black
    ## 2:  2 280-30-2349             index   TRUE 280-30-2349        black
    ## 3:  3 214-05-0872             index   TRUE 214-05-0872        black
    ## 4:  4 712-65-9980             index   TRUE 712-65-9980        black
    ## 5:  5 356-34-3345             index   TRUE 356-34-3345        black
    ##    color.highlight.background color.highlight.border
    ## 1:                     orange                darkred
    ## 2:                     orange                darkred
    ## 3:                     orange                darkred
    ## 4:                     orange                darkred
    ## 5:                     orange                darkred

``` r
visnet <- visNetwork(nodes, edges) %>%
    visGroups(groupname = "index", color = "firebrick", shape = "diamond") %>%
    visGroups(groupname = "index_and_contact", color = "gold", shape = "triangle") %>%
    visGroups(groupname = "contact_only", color =  "steelblue", shape = "circle") %>%
    visLegend(position = "right", main = "Sample index and contact")

visnet
```
