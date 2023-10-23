export default interface DataMapper<Domain, Persistence> {
    toPersistence(d: Domain): Promise<Persistence>
    toDomain(p: Persistence): Promise<Domain>
}
