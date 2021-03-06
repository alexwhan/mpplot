#' Make a pedigree plot
#'
#' Takes a pedigree data.frame and returns a pedigree plot
#'
#' @param ped A data.frame made up of four
#' variables, id, Female, Male, Observed.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes
#'

make_ped_plot <- function(ped) {
  ped <- ped %>%
    mpMap:::convertped() %>%
    dplyr::mutate(generation = make_gen(.)) %>%
    dplyr::group_by(generation) %>%
    dplyr::mutate(rel_id = row_number() / (n() - 1),
           rel_id = rel_id - rel_id[1])

  ped_g <- ped %>%
    dplyr::filter(generation > 1) %>%
    tidyr::gather(parent, parent_id, Female, Male) %>%
    dplyr::rename(progeny_id = id) %>%
    dplyr::mutate(join_id = 1:n()) %>%
    tidyr::gather(type, id, progeny_id, parent_id) %>%
    dplyr::mutate(generation = ifelse(type == "parent_id", generation - 1, generation))

  ped_progeny <- ped_g %>%
    dplyr::filter(type == "progeny_id")

  ped_parent <- ped_g %>%
    dplyr::filter(type == "parent_id") %>%
    dplyr::select(-rel_id) %>%
    dplyr::left_join(ped %>% dplyr::select(id, rel_id))

  ped_path <- bind_rows(ped_parent, ped_progeny)

  ped_plot <- ggplot2::ggplot(ped, aes(rel_id, generation)) + ggplot2::geom_point() +
    ggplot2::geom_line(data = ped_path, aes(group = join_id, colour = parent), alpha = 0.5) +
    ggplot2::scale_y_reverse() +
    ggplot2::geom_text(data = dplyr::filter(ped, generation == 1), aes(label = id, y = generation - 0.05)) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())
  return(ped_plot)
}
