export interface IBuildingEditDTO {
    code: string
    name?: string
    description?: string
    maxFloorDimensions?: { length: number; width: number }
}
