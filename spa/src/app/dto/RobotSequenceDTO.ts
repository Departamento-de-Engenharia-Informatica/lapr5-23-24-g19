type Position = {
    building: string
    floor: number
    x: number
    y: number
}

type TaskPoints = {
    start: Position
    end: Position
    taskId: string
}

export interface RobotSequenceDTO {
    robotName: string
    tasks: {
        cost: number
        initialPosition: Position
        order: TaskPoints[]
    }
}
