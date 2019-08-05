nonequality_fun <- function(x) {
    switch(
        x,
        LT  = `<`,
        LTE = `<=`,
        GT  = `>`,
        GTE = `>=`
    )
}

onerosity_test <- function(contract_details, onerosity_criteria) {
    onerosity_criteria <- transpose(onerosity_criteria) %>% map_depth(1, discard, is.na)
    
    for (oc in onerosity_criteria) {
        oc_type <- names(oc)[2]
        contract_details <- mutate(contract_details,
                                   !!oc_type := ifelse(nonequality_fun(oc[[2]])(AGE, oc$AGE), TRUE, FALSE))
    }
    mutate(contract_details,
           DOUBTFUL = !(ONEROUS & NON_ONEROUS),
           group_type = case_when(
               ONEROUS ~ "onerous",
               NON_ONEROUS ~ "non_onerous",
               DOUBTFUL ~ "doubtful"
           ))
}

rowCallback_bs <- c(
    "function(row, dat, displayNum, index){",
    "  if(index == 0 || index == 4 || index == 5 || index == 10 || index == 11 || index == 12 || index == 14){",
    "    for(var j=0; j<dat.length; j++){",
    "      $('td:eq('+j+')', row)", 
    "        .css('font-weight', 'bold')",
    "    }",
    "  }",
    "  if(index == 4 || index == 10 || index == 11 || index == 14){",
    "    for(var j=0; j<dat.length; j++){",
    "      $('td:eq('+j+')', row)", 
    "        .css('border-bottom', '1px solid black')",
    "    }",
    "  }",
    "DTWidget.formatRound(this, row, dat, 1, 0, 3, ',', '.')",
    "}"
)

rowCallback_is <- c(
    "function(row, dat, displayNum, index){",
    "  if(index == 2 || index == 9 || index == 10 || index == 11 || index == 13 || index == 14){",
    "    for(var j=0; j<dat.length; j++){",
    "      $('td:eq('+j+')', row)", 
    "        .css('font-weight', 'bold')",
    "    }",
    "  }",
    "  if(index == 1 || index == 2 || index == 8 || index == 9 || index == 10 || index == 13 || index == 14){",
    "    for(var j=0; j<dat.length; j++){",
    "      $('td:eq('+j+')', row)", 
    "        .css('border-bottom', '1px solid black')",
    "    }",
    "  }",
    "DTWidget.formatRound(this, row, dat, 1, 0, 3, ',', '.')",
    "var value=dat[1]; $(this.api().cell(row, 1).node()).css({'color':isNaN(parseFloat(value)) ? '' : value <= -1e-5 ? 'red' : 'black'})",
    "}"
)
