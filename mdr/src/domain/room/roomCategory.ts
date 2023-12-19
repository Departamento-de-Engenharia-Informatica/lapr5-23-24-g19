import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

enum CategoryType {
    GABINETE = 'GABINETE',
    ANFITEATRO = 'ANFITEATRO',
    LABORATORIO = 'LABORATORIO',
    OUTRO = 'OUTRO',
}
interface Props {
    category: CategoryType
}

export class RoomCategory extends ValueObject<Props> {
    get value(): string {
        return this.props.category
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(category: string): Result<RoomCategory> {
        let type: CategoryType

        switch (category.trim().toUpperCase()) {
            case CategoryType.GABINETE:
                type = CategoryType.GABINETE
                break
            case CategoryType.LABORATORIO:
                type = CategoryType.LABORATORIO
                break
            case CategoryType.ANFITEATRO:
                type = CategoryType.ANFITEATRO
                break
            default:
                type = CategoryType.OUTRO
        }

        return Result.ok(new RoomCategory({ category: type }))
    }
}
