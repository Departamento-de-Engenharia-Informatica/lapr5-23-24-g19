export interface CreateBuildingDTO {
    code: string
    name?: string
    description?: string
    maxFloorDimensions: {
        length: number
        width: number
    }
}
