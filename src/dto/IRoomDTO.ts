export interface IRoomDTO {
    buildingCode: string
    floor: number
    name: string
    description: string
    category: string
    roomDimensions: { length: number; width: number }
    roomPositions: { x_axis: number; y_axis: number }
}
