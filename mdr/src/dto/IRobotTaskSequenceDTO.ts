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

export interface IRobotSequenceDTO {
    robotName: string
    tasks: {
        cost: number
        initialPosition: Position
        order: TaskPoints[]
    }
}

export type IRobotTaskSequenceDTO = IRobotSequenceDTO[]
