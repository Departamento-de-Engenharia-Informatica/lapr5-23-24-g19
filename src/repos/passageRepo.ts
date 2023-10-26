import { Service, Inject } from 'typedi'
import { Document, Model } from 'mongoose'
import { IPassagePersistence } from '../dataschema/IPassagePersistence'
import IPassageRepo from '../services/IRepos/IPassageRepo'
import { Passage } from '../domain/passage/passage'
import { PassageMap } from '../mappers/PassageMap'
import { BuildingCode } from '../domain/building/buildingCode'
import { IPassageDTO } from '../dto/IPassageDTO'
import { Floor } from '../domain/floor/floor'

@Service()
export default class PassageRepo implements IPassageRepo {
    private models: any

    constructor(@Inject('passageSchema') private passageSchema: Model<IPassagePersistence & Document>) {}

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(passage: Passage | string): Promise<boolean> {
        if (passage instanceof Passage) {
            const query = {
                floor1ID: passage.props.floor1.id.toString(),
                floor2ID: passage.props.floor2.id.toString(),
            }
            const passageDocument = await this.passageSchema.findOne(query)

            try {
                if (!!passageDocument) {
                    return true
                }

                //ordem 1 nao existe
                const query2 = {
                    floor1ID: passage.props.floor2.id.toString(),
                    floor2ID: passage.props.floor1.id.toString(),
                }
                const passageDocument2 = await this.passageSchema.findOne(query2)

                return !!passageDocument2
            } catch (error) {
                throw error
            }
        }
        return false
    }

    public async save(passage: Passage): Promise<Passage> {
        try {
            if (await this.exists(passage)) {
                return null

                // passageDocument.floor1ID = passage.props.floor1.id.toString()
                // passageDocument.floor2ID = passage.props.floor2.id.toString()

                // await passageDocument.save()
                // return passage
            }

            const rawPassage: any = PassageMap.toPersistence(passage)

            const passageCreated = await this.passageSchema.create(rawPassage)

            return PassageMap.toDomain(passageCreated)
        } catch (err) {
            throw err
        }
    }

    public async findAll(): Promise<Passage[]> {
        const records = await this.passageSchema.find()

        if (records.length === 0) {
            return [] // Return an empty array when there are no records
        }

        const passageList = await Promise.all(records.map(async (record) => await PassageMap.toDomain(record)))

        return passageList
    }
    public async passagesBetweenBuildings(
        dto: Passage[],
        codex: BuildingCode,
        codey: BuildingCode,
    ): Promise<Passage[]> {
        try {
            const buildingCode1 = codex.value
            const buildingCode2 = codey.value

            const passages = dto.filter(
                (info) =>
                    (info.props.floor1.building.code.value === buildingCode1 && info.props.floor2.building.code.value === buildingCode2) ||
                    (info.props.floor1.building.code.value === buildingCode2 && info.props.floor2.building.code.value === buildingCode1),
            ).map(info => info.id.toString())

            const doc = await this.passageSchema.find({
                domainID: { $in: passages }
            })

            const passageList = await Promise.all(doc.map(async (record) => await PassageMap.toDomain(record)))

            return passageList
        } catch (error) {
            throw error
        }
    }

    async floorsWithPassage(buildingFloors: Floor[]): Promise<Floor[]> {
        type QueryResult = { matchedField: string }

        const floorIDs = buildingFloors.map((f) => f.id.toString())

        const results: QueryResult[] = await this.passageSchema.aggregate([
            {
                $match: {
                    $or: [{ floor1ID: { $in: floorIDs } }, { floor2ID: { $in: floorIDs } }],
                },
            },
            {
                $project: {
                    _id: 0, // Exclude _id from result
                    matchedField: {
                        $cond: {
                            if: { $in: ['$floor1ID', floorIDs] },
                            then: '$floor1ID',
                            else: '$floor2ID',
                        },
                    },
                },
            },
            {
                $group: {
                    _id: '$matchedField',
                    matchedField: { $first: '$matchedField' },
                },
            },
        ])

        return results.map(({ matchedField: floorId }) => buildingFloors.find((f) => f.id.toString() == floorId))
    }

    async find(floor1: Floor, floor2: Floor): Promise<Passage> {
        return (
            (await this.passageSchema.findOne({
                floor1ID: floor1.id.toString(),
                floor2ID: floor2.id.toString(),
            })) ??
            (await this.passageSchema.findOne({
                floor1ID: floor2.id.toString(),
                floor2ID: floor1.id.toString(),
            }))
        )
    }
}
