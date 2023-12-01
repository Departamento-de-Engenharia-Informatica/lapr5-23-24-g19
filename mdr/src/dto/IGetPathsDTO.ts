export interface IGetPathsDTO {
    criteria: string

    roomStart: {
        building: string
        floor: number
        name: string
    }

    roomGoal: {
        building: string
        floor: number
        name: string
    }
}
