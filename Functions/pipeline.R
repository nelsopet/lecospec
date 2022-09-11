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

Pipeline <- function(
    pipeline_stages, 
    pipeline_options = NULL
){
    pipeline <- list(
        stages = pipeline_stages,
        options = pipeline_options,
        length = length(pipeline_stages)
    )
}

add_stage <- function(pipeline, state){
    
}

apply_stage