import { AggregateRoot } from '../../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../../core/domain/UniqueEntityID'
import { Guard } from '../../../core/logic/Guard'
import { Result } from '../../../core/logic/Result'
import { Role } from '../../role'
import { Email } from '../email'
import { Name } from '../name'
import { PhoneNumber } from '../phoneNumber'
import { UserPassword } from '../userPassword'

interface Props {
    email: Email
    role: Role
    name: Name
    phoneNumber: PhoneNumber
    password: UserPassword
}

export default class BackofficeUser extends AggregateRoot<Props> {
    private constructor(props: Props, id?: UniqueEntityID) {
        super(props, id)
    }

    static create(props: Props, id?: UniqueEntityID): Result<BackofficeUser> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.email, argumentName: 'Email' },
            { argument: props.role, argumentName: 'Role' },
            { argument: props.name, argumentName: 'Name' },
            { argument: props.phoneNumber, argumentName: 'PhoneNumber' },
            { argument: props.password, argumentName: 'Password' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message ?? '')
        }

        return Result.ok(new BackofficeUser({ ...props }, id))
    }

    get id(): UniqueEntityID {
        return this._id
    }

    get name() {
        return this.props.name
    }

    get email() {
        return this.props.email
    }

    get phoneNumber() {
        return this.props.phoneNumber
    }

    get role() {
        return this.props.role
    }
}
