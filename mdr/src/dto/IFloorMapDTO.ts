export interface IFloorMapDTO {
    buildingCode: string
    floorNumber: number
    map?: {
        dimensions: { length: number; width: number }
        //     mapContent: number[][]
        //     passages: {
        //         x: number,
        //         y: number,
        //         orientation: string,
        //         to: {
        //             building: string,
        //             floor: number
        //         }
        //     }[]
        //     rooms: {
        //         x: number,
        //         y: number,
        //         orientation: string,
        //         name: string
        //     }[]
        //     elevators: {
        //         x: number,
        //         y: number,
        //         orientation: string,
        //         floors: number[]
        //     }[]
    }
    path: string
}
