class Car < Object begin
      def init(make, model, year, miles)
        @make = make;
	@model = model;
	@year = year;
	@miles = miles
      end

      def make() @make end
      def model() @model end
      def year() @year end
      def miles() @miles end

      def add_miles(a) @miles = @miles.+(a) end

      def print_car()
        space = " ";
	str = "miles";
	with = "with";
	@year.to_s().print();
	space.print();
	@make.print();
	@model.print();
	with.print();
	space.print();
	@miles.to_s().print();
	space.print();
	str.print();
	space.print()
      end

end

car = new Car;
car.init("Toyota ", "Camry ", 2003, 100000);
car.add_miles(54000);
car.print_car()