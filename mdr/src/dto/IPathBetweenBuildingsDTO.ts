export interface IPathBetweenBuildingsDTO {
    criteria: string

    start: {
        building: string
        floor: number
        coordinates: {
            x: number
            y: number
        }
    }

    goal: {
        building: string
        floor: number
        coordinates: {
            x: number
            y: number
        }
    }
}
