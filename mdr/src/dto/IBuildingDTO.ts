export interface IBuildingDTO {
    code: string
    name?: string
    description?: string
    maxFloorDimensions: { length: number; width: number }
}
