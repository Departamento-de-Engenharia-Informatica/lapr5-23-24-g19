export default interface IElevatorPersistence {
    domainId: string

    // building: Building
    building: string
    identifier: number
    // floors: Floor[]
    floors: number[]

    brand?: string
    model?: string
    serialNumber?: string
    description?: string
}
