#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##       R script                                  ##
##                                                             ##
##                                             2018-03-29      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################



#' @title Relation Before
#' @name luccpl_before
#' @aliases luccpl_before
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and instants in which one of the
#' classes in p is present in a time before t[
#'
#' @usage luccpl_before(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. List of patterns used for the query.
#' @param pattern_list2 Not used in this function, as it does not relate different patterns.
#' @param date1 Integer. Date referring to pattetn_list1
#' @param date2 Not used in this function because pattern_list2 is not used
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 1. Relation Before
luccpl_before <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1)
      and_or <- 0
    if (length(pattern_list1) > 1) {
      # or for all connections except the last
      and_or[1:(length(pattern_list1))] <- 1
      and_or[length(pattern_list1)] <- 0
    }
    
    date1[1:length(pattern_list1)] <- date1
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step_number <- tdata(date1, dates)
    
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      # for each pattern of land use a column is created in the query_array
      query_array <-
        c(query_array,
          1,
          0,
          time_step_number[i],
          pattern_number1[i],
          0,
          and_or[i])
    }
    
    return(query_array)
  }





#' @title Relation After
#' @name luccpl_after
#' @aliases luccpl_after
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and instants in which one of the
#' classes in p is present in a time after ]t
#'
#' @usage luccpl_after(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. List of patterns used for the query.
#' @param pattern_list2 Not used in this function, as it does not relate different patterns.
#' @param date1 Integer. Date referring to pattetn_list1
#' @param date2 Not used in this function because pattern_list2 is not used
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 2. Relation After
luccpl_after <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1)
      and_or <- 0
    if (length(pattern_list1) > 1) {
      # or for all connections except the last
      and_or[1:(length(pattern_list1))] <- 1
      and_or[length(pattern_list1)] <- 0
    }
    
    date1[1:length(pattern_list1)] <- date1
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step_number <- tdata(date1, dates)
    
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      # for each pattern of land use a column is created in the query_array
      query_array <-
        c(query_array,
          1,
          time_step_number[i],
          (length(dates) + 1),
          pattern_number1[i],
          0,
          and_or[i])
    }
    
    return(query_array)
  }





#' @title Relation Meets
#' @name luccpl_meets
#' @aliases luccpl_meets
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and moments in which
#' one of the classes in p is present at the same time and before time t]
#'
#' @usage luccpl_meets(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. List of patterns used for the query.
#' @param pattern_list2 Not used in this function, as it does not relate different patterns.
#' @param date1 Integer. Date referring to pattetn_list1
#' @param date2 Not used in this function because pattern_list2 is not used
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 3. Relation Meets
luccpl_meets <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1)
      and_or <- 0
    if (length(pattern_list1) > 1) {
      # or for all connections except the last
      and_or[1:(length(pattern_list1))] <- 1
      and_or[length(pattern_list1)] <- 0
    }
    
    date1[1:length(pattern_list1)] <- date1
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step_number <- tdata(date1, dates)
    
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      # for each pattern of land use a column is created in the query_array
      query_array <-
        c(query_array,
          2,
          0,
          time_step_number[i],
          pattern_number1[i],
          0,
          and_or[i])
    }
    
    return(query_array)
  }





#' @title Relation Met By
#' @name luccpl_metby
#' @aliases luccpl_metby
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and moments in which
#' one of the classes in p is present at the same time and after time ]t
#'
#' @usage luccpl_metby(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. List of patterns used for the query.
#' @param pattern_list2 Not used in this function, as it does not relate different patterns.
#' @param date1 Integer. Date referring to pattetn_list1
#' @param date2 Not used in this function because pattern_list2 is not used
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 4. Relation Met by
luccpl_metby <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1)
      and_or <- 0
    if (length(pattern_list1) > 1) {
      # or for all connections except the last
      and_or[1:(length(pattern_list1))] <- 1
      and_or[length(pattern_list1)] <- 0
    }
    
    date1[1:length(pattern_list1)] <- date1
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step_number <- tdata(date1, dates)
    
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      # for each pattern of land use a column is created in the query_array
      query_array <-
        c(query_array,
          3,
          time_step_number[i],
          (length(dates) + 1),
          pattern_number1[i],
          0,
          and_or[i])
    }
    
    return(query_array)
  }





#' @title Relation Holds
#' @name luccpl_holds
#' @aliases luccpl_holds
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and instants in which
#' one of the classes present in p is contained in all the instants of the interval ]ti, tj[
#'
#' @usage luccpl_holds(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. List of patterns used for the query.
#' @param pattern_list2 Not used in this function, as it does not relate different patterns.
#' @param date1 Integer. Initial date
#' @param date2 Integer. Final date
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 5. Relation Holds
luccpl_holds <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(date2,!is.null(date2),
                         err_desc = "date2, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1)
      and_or <- 0
    if (length(pattern_list1) > 1) {
      # or for all connections except the last
      and_or[1:(length(pattern_list1))] <- 1
      and_or[length(pattern_list1)] <- 0
    }
    
    date1[1:length(pattern_list1)] <- date1
    date2[1:length(pattern_list1)] <- date2
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step1 <- tdata(date1, dates)
    time_step2 <- tdata(date2, dates)
    
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      # for each pattern of land use a column is created in the query_array
      query_array <-
        c(query_array,
          4,
          time_step1[i],
          time_step2[i],
          pattern_number1[i],
          0,
          and_or[i])
    }
    
    return(query_array)
  }





#' @title Relation During
#' @name luccpl_during
#' @aliases luccpl_during
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and instants in which
#' one of the classes present in p is contained in the instants of the interval ]ti, tj[
#'
#' @usage luccpl_during(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. List of patterns used for the query.
#' @param pattern_list2 Not used in this function, as it does not relate different patterns.
#' @param date1 Integer. Initial date
#' @param date2 Integer. Final date
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 6. Relation During
luccpl_during <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(date2,!is.null(date2),
                         err_desc = "date2, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1)
      and_or <- 0
    if (length(pattern_list1) > 1) {
      # or for all connections except the last
      and_or[1:(length(pattern_list1))] <- 1
      and_or[length(pattern_list1)] <- 0
    }
    
    date1[1:length(pattern_list1)] <- date1
    date2[1:length(pattern_list1)] <- date2
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step_number1 <- tdata(date1, dates)
    time_step_number2 <- tdata(date2, dates)
    
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      # for each pattern of land use a column is created in the query_array
      query_array <-
        c(
          query_array,
          1,
          time_step_number1[i],
          time_step_number2[i],
          pattern_number1[i],
          0,
          and_or[i]
        )
    }
    
    return(query_array)
  }





#' @title Relation Recur
#' @name luccpl_recur
#' @aliases luccpl_recur
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and times when one
#' of the classes present in p was recurrent within the range [ti, tj],
#'
#' @usage luccpl_recur(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. List of patterns used for the query.
#' @param pattern_list2 Not used in this function, as it does not relate different patterns.
#' @param date1 Integer. Initial date
#' @param date2 Integer. Final date
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 7. Relation Recur
luccpl_recur <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1)
      and_or <- 0
    if (length(pattern_list1) > 1) {
      # or for all connections except the last
      and_or[1:(length(pattern_list1))] <- 1
      and_or[length(pattern_list1)] <- 0
    }
    
    date1[1:length(pattern_list1)] <- date1
    date2[1:length(pattern_list1)] <- date2
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step1 <- tdata(date1, dates)
    time_step2 <- tdata(date2, dates)
    
    
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      # for each pattern of land use a column is created in the query_array
      query_array <-
        c(query_array,
          5,
          time_step1[i],
          time_step2[i],
          pattern_number1[i],
          0,
          and_or[i])
    }
    
    return(query_array)
  }





#' @title Relation Convert
#' @name luccpl_convert
#' @aliases luccpl_convert
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and times when one of the classes in pi was directly
#' converted to one of the classes in pj within the range [ti, tj];
#'
#' @usage luccpl_convert(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. First list of patterns used for the query.
#' @param pattern_list2 Character. Second list of patterns used for the query
#' @param date1 Integer. Initial date
#' @param date2 Integer. Final date
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 8. Relation Convert
luccpl_convert <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    and_or <- NULL
    
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(pattern_list2,!is.null(pattern_list2),
                         err_desc = "pattern_list2, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(date2,!is.null(date2),
                         err_desc = "date2, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    if (length(pattern_list1) == 1 &&
        length(pattern_list2) == 1)
      and_or <- 0
    else {
      # or for all connections except the last
      and_or[1:(((length(pattern_list1)) * (length(pattern_list2))) - 1)] <-
        1
      and_or[((length(pattern_list1)) * (length(pattern_list2)))] <- 0
    }
    
    #date1[1:((length(pattern_list1))*(length(pattern_list2)))] <- date1
    #date2[1:((length(pattern_list1))*(length(pattern_list2)))] <- date2
    
    date1[1:length(pattern_list1)] <- date1
    date2[1:length(pattern_list2)] <- date2
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    pattern_number2 <- mdata(pattern_list2, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step1 <- tdata(date1, dates)
    time_step2 <- tdata(date2, dates)
    
    query_array <- NULL
    cont <- 0
    for (i in 1:length(pattern_list1)) {
      for (j in 1:length(pattern_list2)) {
        cont <- cont + 1
        # for each pattern of land use a column is created in the query_array
        query_array <-
          c(
            query_array,
            6,
            time_step1[i],
            time_step2[i],
            pattern_number1[i],
            pattern_number2[j],
            and_or[cont]
          )
      }
    }
    
    return(query_array)
  }





#' @title Relation Evolve
#' @name luccpl_evolve
#' @aliases luccpl_evolve
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Classifies as true all the locations and times when one of the classes in pi evolved
#' into one of the classes in pj within the range [ti, tj].
#'
#' @usage luccpl_evolve(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL)
#'
#' @param pattern_list1 Character. First list of patterns used for the query.
#' @param pattern_list2 Character. Second list of patterns used for the query
#' @param date1 Integer. Initial date
#' @param date2 Integer. Final date
#' @param dates Integer. A vector that represents the timeline
#' @param metadata Character. A vector with the land use patterns of the input data
#'
#' @keywords datasets
#' @return A query_array that can be grouped with others to perform the query
#' @importFrom ensurer ensure_that
#' @importFrom LuccPL mdata tdata
#' @export
#'

# 9. Relation Evolve
luccpl_evolve <-
  function(pattern_list1 = NULL,
           pattern_list2 = NULL,
           date1 = NULL,
           date2 = NULL,
           dates = NULL,
           metadata = NULL) {
    # date2 optional (ARRUMAR ISSO)
    and_or <- NULL
    
    ensurer::ensure_that(pattern_list1,!is.null(pattern_list1),
                         err_desc = "pattern_list1, must be defined!")
    ensurer::ensure_that(pattern_list2,!is.null(pattern_list2),
                         err_desc = "pattern_list2, must be defined!")
    ensurer::ensure_that(date1,!is.null(date1),
                         err_desc = "date1, must be defined!")
    ensurer::ensure_that(date2,!is.null(date2),
                         err_desc = "date2, must be defined!")
    ensurer::ensure_that(dates,!is.null(dates),
                         err_desc = "dates, must be defined!")
    ensurer::ensure_that(metadata,!is.null(metadata),
                         err_desc = "metadata, must be defined!")
    
    
    if (length(pattern_list1) == 1 &&
        length(pattern_list2) == 1)
      and_or <- 0
    else {
      # or for all connections except the last
      and_or[1:((length(pattern_list1)) * (length(pattern_list2)))] <-
        1
      and_or[((length(pattern_list1)) * (length(pattern_list2)))] <- 0
    }
    
    
    date1[1:length(pattern_list1)] <- date1
    date2[1:length(pattern_list2)] <- date2
    
    # establishes the relationship between land use type and digital number in the classifications
    pattern_number1 <- mdata(pattern_list1, metadata)
    pattern_number2 <- mdata(pattern_list2, metadata)
    
    # establishes the relationship between date and time-step of each classification
    time_step1 <- tdata(date1, dates)
    time_step2 <- tdata(date2, dates)
    
    cont <- 0
    query_array <- NULL
    for (i in 1:length(pattern_list1)) {
      for (j in 1:length(pattern_list2)) {
        cont <- cont + 1
        # for each pattern of land use a column is created in the query_array
        query_array <-
          c(
            query_array,
            7,
            time_step1[i],
            time_step2[j],
            pattern_number1[i],
            pattern_number2[j],
            and_or[cont]
          )
      }
    }
    
    return(query_array)
  }
