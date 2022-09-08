PipelineStage <- function(
    transform, 
    required_datatype,
    output_datatype,
    additional_data = NULL
){
    pipe_stage <- list(
        transform = transform,
        input_type = required_datatype,
        output_type = output_datatype,
        additional_data = additional_data
    )
}