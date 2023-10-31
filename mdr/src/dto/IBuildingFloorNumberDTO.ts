export interface IBuildingFloorNumberDTO {
    code: string
    name?: string
    description?: string
    maxFloorDimensions: { length: number; width: number }
    floorNumber: number
}
