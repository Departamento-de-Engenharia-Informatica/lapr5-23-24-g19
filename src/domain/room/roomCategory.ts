import { ValueObject } from '../../core/domain/ValueObject'
import { Result } from '../../core/logic/Result'

interface Props {
    category: string
}

export class RoomCategory extends ValueObject<Props> {
    get value(): string {
        return this.props.category
    }

    private constructor(props: Props) {
        super(props)
    }

    public static create(category?: string): Result<RoomCategory> {
        category = category ?? ''

        return Result.ok(new RoomCategory({ category }))
    }
}
