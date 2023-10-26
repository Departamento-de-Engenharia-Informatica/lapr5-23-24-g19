export interface IRobotPersistence {
    domainId: string
    code: string
    nickname: string
    type: string
    serialNumber: string
    state: number
    description?: string
}
