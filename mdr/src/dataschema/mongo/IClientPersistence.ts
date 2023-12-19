export interface IClientPersistence {
    domainId: string
    name: string
    email: string
    phoneNumber: string
    vatNumber: number

    password: string
    // salt: string
}
