export interface ICreatedElevatorDTO {
    buildingId: string
    identifier: number
    floors: number[]

    brand?: string
    model?: string
    serialNumber?: string
    description?: string
}
