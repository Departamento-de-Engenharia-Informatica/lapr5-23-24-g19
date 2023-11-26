export interface EditElevatorDTO {
    buildingId: string
    floors: number[]
    identifier: number

    brand?: string
    model?: string
    serialNumber?: string
    description?: string
}
