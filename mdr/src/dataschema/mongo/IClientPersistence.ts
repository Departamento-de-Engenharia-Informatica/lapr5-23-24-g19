export interface IClientPersistence {
    domainId: string
    name: string
    email: string
    phoneNumber: number
    vatNumber: number

    password: string
    // salt: string
}
