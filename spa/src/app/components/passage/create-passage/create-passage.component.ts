import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { catchError, every, of, tap } from 'rxjs';
import { BuildingDTO } from 'src/app/dto/BuildingDTO';
import { BuildingService } from 'src/app/services/building.service';
import { ErrorMessageService } from 'src/app/services/error-message.service';
import { FloorAndBuildingDTO, FloorService } from 'src/app/services/floor.service';
import { PassageDTO, PassageService } from 'src/app/services/passage.service';

@Component({
	selector: 'app-create-passage',
	templateUrl: './create-passage.component.html',
	styleUrls: ['./create-passage.component.css']
})
export class CreatePassageComponent {
	@Output() formSubmitted = new EventEmitter<any>();

	passageForm: UntypedFormGroup

	buildings: BuildingDTO[] = []

	building1!: BuildingDTO
	building2!: BuildingDTO

	floor1!: FloorAndBuildingDTO
	floor2!: FloorAndBuildingDTO
	floors1!: FloorAndBuildingDTO[];
	floors2!: FloorAndBuildingDTO[];

	constructor(private fb: FormBuilder,
		private buildingService: BuildingService,
		private floorService: FloorService,
		private message: ErrorMessageService,
		private passageService: PassageService) {

		this.passageForm = this.fb.group({
			building1: [null, Validators.required],
			building2: [null, Validators.required],
			floor1: [null, Validators.required],
			floor2: [null, Validators.required],
		});
	}

	submitForm() {

		if (this.passageForm.value.building1 == this.passageForm.value.building2) {
			console.log("Can not be the same building")
			// send error message(deveria fazer o pedido para o backend e se der erro mostrar?)
		}
		console.log("BUilding1", this.passageForm.value.building1)
		console.log("BUilding2", this.passageForm.value.building2)
		console.log("floor1", this.passageForm.value.floor1)
		console.log("floor2", this.passageForm.value.floor2)
		const dto = {
			floor1: {
				buildingCode: this.passageForm.value.building1,
				floorNumber: this.passageForm.value.floor1
			},
			floor2: {
				buildingCode: this.passageForm.value.building2,
				floorNumber: this.passageForm.value.floor2
			}

		} as PassageDTO
		this.passageService.postPassage(dto).subscribe((passage: PassageDTO) => {
			console.log("Passage created")
			// this.message.setSucessMessage("Passage created")
		})
	}

	onBuilding1Selected(event: any) {
		this.floorService
			.getFloors(event.target.value as string)
			.pipe(
				tap((list: FloorAndBuildingDTO[]) => {
					this.floors1 = list;
				}),
				catchError((error) => {
					if (error.status === 404) {
						this.floors1 = [];
					} else {
						console.error('Error fetching floors:', error);
					}
					return of();
				})
			).subscribe();

	}

	onBuilding2Selected(event: any) {
		this.floorService
			.getFloors(event.target.value as string)
			.pipe(
				tap((list: FloorAndBuildingDTO[]) => {
					this.floors2 = list;
				}),
				catchError((error) => {
					if (error.status === 404) {
						this.floors2 = [];
					} else {
						// this.message.setErrorMessage(error)
						console.error('Error fetching floors:', error);
					}
					return of();
				})
			).subscribe();
	}

	ngOnInit(): void {
		this.buildingService.getBuildings().subscribe((buildingsList: BuildingDTO[]) => {
			this.buildings = buildingsList;
		});
	}

	getBuilding(buildingCode: string): BuildingDTO | undefined {
		return this.buildings.find(building => building.code === buildingCode);
	}

	isInvalid(controlName: string): boolean {
		const control = this.passageForm.get(controlName);
		return (
			!!control && control.invalid && (control.dirty || control.touched)
		);
	}
	noFloors1() {
		return  this.floors1!=null && this.floors1!=undefined && this.floors1.length == 0
	}
	noFloors2() {
		return this.floors2!=null && this.floors2!=undefined && this.floors2.length == 0
	}

}
