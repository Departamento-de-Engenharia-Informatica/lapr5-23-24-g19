import {
    ChangeDetectionStrategy,
    Component,
    DoCheck,
    EventEmitter,
    Input,
    OnChanges,
    OnInit,
    Output
} from '@angular/core';
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import {
    BuildingDTO,
    BuildingService,
} from 'src/app/services/building.service';
import {
    ElevatorDTO,
    ElevatorService,
} from 'src/app/services/elevator.service';
import { FloorDTO, FloorService } from 'src/app/services/floor.service';

@Component({
    selector: 'app-create-elevator',
    templateUrl: './create-elevator.component.html',
    styleUrls: ['./create-elevator.component.css'],
})
export class CreateElevatorComponent implements OnInit,OnChanges{

    @Input() selectedBuilding: string;
    @Input() selectedFloors: string[];
    @Input() createdElevator;

    @Output() formSubmitted = new EventEmitter<any>();

    elevatorForm: UntypedFormGroup;

    buildings: BuildingDTO[] = [];
    floors: FloorDTO[] = [];
    elevator: ElevatorDTO;

    constructor(
        private fb: FormBuilder,
        private buildingService: BuildingService,
        private floorService: FloorService,
        private elevatorService: ElevatorService,
    ) {

        this.selectedBuilding = '';
        this.selectedFloors = [];
        this.createdElevator = null as unknown as ElevatorDTO;
        this.elevator =  null as unknown as ElevatorDTO;
        this.buildings = [];
        this.floors = [];

        this.elevatorForm = this.fb.group({
            selectedBuilding: ['', Validators.required],
            selectedFloors: [[], Validators.required],
            brand: ['', Validators.required],
            model: ['', Validators.required],
            serialNumber: ['', Validators.required],
            description: [''],
        });


        /*this.elevatorForm.get('selectedBuilding')?.valueChanges.subscribe(value => {
            console.log('Selected Building Changed:', value);
        });

        this.elevatorForm.get('selectedFloors')?.valueChanges.subscribe(value => {
            console.log('Selected Floors Changed:', value);
        });*/

    }



    ngOnChanges(): void {


        this.elevatorForm.get('selectedBuilding')?.valueChanges.subscribe((selectedBuilding) => {
            console.log('Selected Building Changed:', selectedBuilding);
            if (selectedBuilding.length !== 0) {
                this.floorService.getFloors(selectedBuilding).subscribe((list: FloorDTO[]) => {
                    this.floors = list;

                });
            }
        });
    /*console.log("ola",this.selectedBuilding.length);
        if (this.selectedBuilding.length !== 0) {
            this.floorService
                .getFloors(this.selectedBuilding)
                .subscribe((list: FloorDTO[]) => {
                    this.floors = list;
                    console.log("Floors:", this.floors);
                });
        }*/
    }

    ngOnInit(): void {
        this.buildingService.getBuildings().subscribe((list: BuildingDTO[]) => {
            this.buildings = list;
        });

        this.ngOnChanges();

    }




    submitElevatorForm() {

        if (this.elevatorForm.valid) {
            // Set values to the properties
            const selectedBuilding = this.elevatorForm.get(
                'selectedBuilding',
            )?.value;
            const selectedFloors =
                this.elevatorForm.get('selectedFloors')?.value;
            const brand = this.elevatorForm.get('brand')?.value;
            const model = this.elevatorForm.get('model')?.value;
            const serialNumber = this.elevatorForm.get('serialNumber')?.value;
            const description = this.elevatorForm.get('description')?.value;

            const elevator: ElevatorDTO = {
                buildingId: selectedBuilding,
                floors: selectedFloors,
                brand: brand,
                model: model,
                serialNumber: serialNumber,
                description: description,
            };




            this.elevator = elevator;
            this.createdElevator = elevator;
            this.elevatorService.createElevator(elevator);
            this.formSubmitted.emit(elevator);


            // Reset the form
            this.elevatorForm.reset();


        }
    }


}
