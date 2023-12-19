export interface IFloorPersistence {
    domainId: string
    buildingCode: string
    floorNumber: number
    description: string
    path?: string
    // map?:{
    // dimensions:{
    //     length: number
    //     width: number
    // }

    // mapContent: number[][]
    // passages?: {
    //     x:number,
    //     y:number
    // }[]
    // rooms?: {
    //     x:number,
    //     y:number
    // }[]
    // elevators?: {
    //     x:number,
    //     y:number
    // }[]
    // }
}
