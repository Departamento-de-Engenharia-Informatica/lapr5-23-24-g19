import { AggregateRoot } from '../../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../../core/domain/UniqueEntityID'
import { Guard } from '../../../core/logic/Guard'
import { Result } from '../../../core/logic/Result'
import { Email } from '../email'
import { Name } from '../name'
import { PhoneNumber } from '../phoneNumber'
import { UserPassword } from '../userPassword'
import { ClientStatus } from './status'
import { VatNumber } from './vatNumber'

interface CreateProps {
    email: Email
    name: Name
    phoneNumber: PhoneNumber
    vatNumber: VatNumber

    password: UserPassword
}

interface Props extends CreateProps {
    status: ClientStatus
}

export default class Client extends AggregateRoot<Props> {
    private constructor(props: Props, id?: UniqueEntityID) {
        super(props, id)
    }

    static create(props: CreateProps, id?: UniqueEntityID): Result<Client> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.email, argumentName: 'Email' },
            { argument: props.name, argumentName: 'Name' },
            { argument: props.phoneNumber, argumentName: 'PhoneNumber' },
            { argument: props.vatNumber, argumentName: 'VATnumber' },
            { argument: props.password, argumentName: 'Password' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message ?? '')
        }

        return Result.ok(new Client({ status: ClientStatus.PENDING, ...props }, id))
    }

    get id(): UniqueEntityID {
        return this._id
    }

    get name() {
        return this.props.name
    }

    set name(newName: Name) {
        this.props.name = newName
    }

    get email() {
        return this.props.email
    }

    get phoneNumber() {
        return this.props.phoneNumber
    }

    set phoneNumber(newPhoneNumber: PhoneNumber) {
        this.props.phoneNumber = newPhoneNumber
    }

    get vatNumber() {
        return this.props.vatNumber
    }

    set vatNumber(newVatNumber: VatNumber) {
        this.props.vatNumber = newVatNumber
    }

    get status() {
        return this.props.status
    }

    set status(newStatus: ClientStatus) {
        this.props.status = newStatus
    }

    _get_pwd() {
        return this.props.password
    }
}
