tests = ["rmath", "generic", "misc", "chainrules", "inverse", "tvpack"]

for t in tests
    fp = "$t.jl"
    println("* running $fp")
    include(fp)
end

using JET
@testset "Static analysis" begin
    @test isempty(JET.get_reports(report_package("StatsFuns", target_modules=(StatsFuns,))))
end
