export interface CreateRobotTypeDTO {
    code: string
    brand: string
    model: string
    taskTypes: string[]
}

export interface TaskTypeDTO {
    description: string
}
