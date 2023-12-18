import { AggregateRoot } from "../../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../../core/domain/UniqueEntityID";
import { Guard } from "../../../core/logic/Guard";
import { Result } from "../../../core/logic/Result";
import { Email } from "../email";
import { Name } from "../name";
import { PhoneNumber } from "../phoneNumber";
import { UserPassword } from "../userPassword";
import { VatNumber } from "./vatNumber";

interface Props {
    email: Email
    name: Name
    phone: PhoneNumber
    vatNumber: VatNumber

    password: UserPassword
}

export default class Client extends AggregateRoot<Props> {
    private constructor(props: Props, id?: UniqueEntityID) {
        super(props, id)
    }

    static create(props: Props, id?: UniqueEntityID) {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.email, argumentName: 'Email' },
            { argument: props.name, argumentName: 'Name' },
            { argument: props.phone, argumentName: 'PhoneNumber' },
            { argument: props.vatNumber, argumentName: 'VATnumber' },
            { argument: props.password, argumentName: 'Password' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message ?? '')
        }

        return Result.ok(new Client({ ...props }, id))
    }
}
